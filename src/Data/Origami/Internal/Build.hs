-- | Builds fold declarations for a 'FoldFamily'.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Origami.Internal.Build(
    buildFolds,
    buildFoldsDryRun,
    -- * for testing
    BuildErr,
    buildFoldFamilyMaybe
) where

import Control.Applicative(Applicative(..))
import Control.Lens hiding (Fold)
import Control.Monad.Error(Error(..), ErrorT(..), MonadError(..))
import Control.Monad.RWS(MonadReader(..), MonadState(..), MonadWriter(..),
    RWST(..), evalRWST, gets, lift, liftM, liftM2, liftM3,  modify, unless,
    when)
import Data.Bitraversable(Bitraversable)
import qualified Data.Data as D
import Data.List(intercalate)
import qualified Data.Map as M
import Data.Origami.Internal.Fold(foldFoldFamily)
import Data.Origami.Internal.FoldFamily
import Data.Origami.Internal.Pretty(prettyFold)
import Data.Origami.Internal.TH(duplicateCtorNames, mkFoldDecs)
import Data.Origami.Internal.THUtils(unAppTs, upperName)
import Data.Origami.Internal.Trifunctor(Tritraversable)
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.Quote(dataToExpQ)

-- | Prints onto standard output the result of running 'buildFolds'
-- but doesn't splice the fold declarations.
buildFoldsDryRun :: [Name] -> [Name] -> [Name] -> Q [Dec]
buildFoldsDryRun rts functs atoms = do
    ff <- buildFoldFamily rts functs atoms
    runIO $ do
        print $ foldFoldFamily prettyFold ff
        print $ ppr $ mkFoldDecs ff
    return []

-- | Discovers the fold family and builds declarations from it to be
-- spliced into a source file.
--
-- The fold family includes the root datatypes and the datatypes of
-- all of their components, recursively. Datatypes declared as atomic
-- will not be included, nor their components.
--
-- In general, the framework does not currently handle parameterized
-- datatypes, but applications of datatypes in 'Traversable',
-- 'Bitraversable', or 'Tritraversable' are treated as "transparent"
-- and traversed through.
--
-- The framework generates:
--
-- * a type-parameterized @Fold@ record datatype. Each type parameter
-- @xxx@ corresponds to a non-atomic datatype @Xxx@ in the fold
-- family. Each field @mkYyy@ of the @Fold@ corresponds to a
-- constructor @Yyy@ used by some datatype in the fold family.
--
-- * an @idFold@ record. Folding over @idFold@ is equivalent to
-- applying @id@: it does nothing. @idFold@ is useful as a base record
-- to build your own folds upon.
--
-- * an @errFold@ function to create a @Fold@ record, with undefined
-- fields that give a useful error message when accessed. The @mkXxx@
-- field of @errFold "example"@ is defined to contain @error
-- "example.mkXxx"@.
--
-- * a @monadicFold@ function that lifts a @Fold a b c@ into a @Fold
-- (m a) (m b) (m c)@. It applies the base fold monadically in a
-- bottom-up, left-to-right way.
--
-- * for each datatype @Xxx@, a @foldXxx@ function that applies a
-- @Fold@ to an @Xxx@ value, returning a value of type @xxx@.
--
-- The names @Fold@, @idFold@, @errFold@, and @monadicFold@ are fixed.
-- They are intended to be imported qualified.
--
-- You are not expected to understand the structure of the generated
-- code from this generic description. Generate code for your specific
-- case and look at its Haddock documentation.
--
-- Since the discovery process can automatically collect a very large
-- number of datatypes, and since the user doesn't usually see the
-- spliced code, we require the user to declare what she expects so
-- that there are no surprises.  For that reason, any functor classes
-- expected to be appear in the result must be declared, as are
-- datatypes the user wants to treat as atomic.
--
-- There are a few other restrictions not mentioned here: if you hit
-- any of them, the framework should output a helpful, intelligible
-- error message when generating the declarations and before trying to
-- splice and compile the declarations. You should see no errors from
-- the compiler trying to compile bad generated code. If you do,
-- that's a bug; please let us know. If the error messages are opaque,
-- that's a bug too.

buildFolds :: [Name]    -- ^ names of the root datatypes
           -> [Name]    -- ^ names of the /n/-ary functor classes to be used
           -> [Name]    -- ^ names of datatypes declared to be atomic
           -> Q [Dec]
buildFolds rts functs atoms = do
    ff <- buildFoldFamily rts functs atoms
    return $ mkFoldDecs ff

-- | Builds a 'FoldFamily'.
buildFoldFamily :: [Name] -> [Name] -> [Name] -> Q FoldFamily
buildFoldFamily rts functs atoms = do
    e <- runBuild $ buildFoldFamilyMB rts functs atoms
    case e of
        Left err -> fail $ show err
        Right ff -> return ff

-- | Builds a 'FoldFamily' or returns an error.  Spliced result is of
-- type 'Either' 'BuildErr' 'FoldFamily'.
buildFoldFamilyMaybe :: [Name] -> [Name] -> [Name] -> Q Exp
buildFoldFamilyMaybe rts functs atoms = do
    e <- recover (return $ Left ErrThrownInQ)
             $ runBuild
                 $ buildFoldFamilyMB rts functs atoms

        -- TODO Extra handling for unique names would go here instead
        -- of (const Nothing).

    dataToExpQ (const Nothing) e

-- | Builds a 'FoldFamily' in any 'MonadBuild' monad.
buildFoldFamilyMB :: forall m . MonadBuild m
    => [Name] -> [Name] -> [Name] -> m FoldFamily

    -- TODO Note that this code is basically a monadic fold over
    -- compiler structures: "Language.Haskell.TH.Syntax".  Monadic,
    -- because we need to call 'reify'.  Could this be rewritten as a
    -- 'Fold'?

buildFoldFamilyMB rts functs atoms = do
    ((), w) <- getData runDfsM
    case processData w of
        Left err -> throwErr err
        Right ff -> return ff

    where
    -- | Runs a depth-first search.
    runDfsM :: m ()
    runDfsM = do
        mapM_ see atoms
        mapM_ visitNm rts

    -- | Visit a 'Name'.
    visitNm :: Name -> m ()
    visitNm nm = do
        s <- seen nm
        unless s $ withStackTop nm $ do
            see nm
            dcs <- getDataCases nm
            putDataTy nm dcs

    -- | Gets a list of 'DataCase's from a type's 'Name' using the
    -- compiler's knowledge.
    getDataCases :: Name -> m [DataCase]
    getDataCases nm = do
        info <- reifyTypeName nm
        case info of
            TyConI dec -> getDataCasesFromDec nm dec
            _ -> throwErrWithStack $ ErrReify nm info

    -- | Gets a list of 'DataCase's from a 'Dec'.
    getDataCasesFromDec :: Name -> Dec -> m [DataCase]
    getDataCasesFromDec nm dec = case dec of
        DataD _ nm' [] cons' _
            -> getDataCasesFromDataD nm' cons'
        DataD {} -> throwErrWithStack $ ErrParamType (pretty dec)
        NewtypeD _ nm' [] con _
            -> getDataCasesFromDataD nm' [con]
        NewtypeD {} -> throwErrWithStack $ ErrParamType (pretty dec)
        TySynD nm' [] ty
            -> withStackTop nm' $ case unAppTs ty of
                  [ConT _nm] -> throwErrWithStack $ ErrUnimpl $ concat [
                      "getDataCasesFromDec ", pretty nm, " ", pretty dec]
                  _ -> throwErrWithStack $ ErrParamType (pretty dec)
        TySynD nm' _ _
            -> withStackTop nm'
                   $ throwErrWithStack
                       $ ErrParamTypeSyn (pretty dec)
        _ -> throwErrWithStack $ ErrReify' nm (pretty dec)

    -- | Gets a list of 'DataCase's from a 'DataD' (or equivalently, a
    -- 'NewtypeD').
    getDataCasesFromDataD :: Name -> [Con] -> m [DataCase]
    getDataCasesFromDataD nm' cons' = if null cons'
            then throwErrWithStack $ ErrEmptyData nm'
            else mapM getDataCasesFromCon cons'

    -- | Gets a 'DataCase' from a 'Con'.
    getDataCasesFromCon :: Con -> m DataCase
    getDataCasesFromCon con = case con of
        NormalC nm' sts -> do
            dfs <- mapM (getDataFieldFromType . snd) sts
            return $ DataCase nm' dfs
        RecC nm' vsts -> do
            dfs <- mapM (getDataFieldFromType . thd3) vsts
            return $ DataCase nm' dfs
        InfixC _ nm' _ -> throwErrWithStack $ ErrInfixCtor nm'
        ForallC {} -> throwErrWithStack
                          $ ErrUnsupported
                                "Universally quanitified constructors"

        where
        thd3 :: (a, b, c) -> c
        thd3 (_, _,c) = c

    -- | Gets a 'DataField' from a 'Type'.
    getDataFieldFromType :: Type -> m DataField
    getDataFieldFromType t = case unAppTs t of
        [ConT nm'] -> getDataFieldFromConstructor nm'
        [ConT nm', t1] -> getDataFieldFromFunctApp nm' t1
        [ListT, t1] -> getDataFieldFromFunctApp ''[] t1
        [ConT nm', t1, t2] -> getDataFieldFromBifunctApp nm' t1 t2
        [TupleT 2, t1, t2] -> getDataFieldFromBifunctApp ''(,) t1 t2
        [ConT nm', t1, t2, t3] -> getDataFieldFromTrifunctApp nm' t1 t2 t3
        [TupleT 3, t1, t2, t3] -> getDataFieldFromTrifunctApp ''(,,) t1 t2 t3
        (ConT nm' : _) -> do
            info <- reifyTypeName nm'
            case info of
                TyConI dec -> case dec of
                    DataD {} -> throwErrWithStack $ ErrParamType (pretty dec)
                    NewtypeD {}
                        -> throwErrWithStack $ ErrParamType (pretty dec)
                    TySynD nm _ _
                        -> withStackTop nm
                               $ throwErrWithStack
                                   $ ErrParamTypeSyn (pretty dec)
                    _ -> throwErrWithStack $ ErrReify' nm' (pretty dec)
                _ -> throwErrWithStack $ ErrReify nm' info
        _ -> throwErrWithStack
                 $ ErrUnimpl ("getDataFieldFromType " ++ pretty t)

    -- | Gets a 'DataField' from a 'ConT' 'Type'.
    getDataFieldFromConstructor :: Name -> m DataField
    getDataFieldFromConstructor nm' = if nm' `elem` atoms
        then return $ Atomic $ Ty nm'
        else do
            mNmTy <- getTypeSynDef nm'
            case mNmTy of
                Just (nm'', t) -> withStackTop nm'' $ getDataFieldFromType t
                Nothing -> do
                    visitNm nm'
                    return $ Nonatomic (Ty nm')

    -- | Gets a 'DataField' from a 'Functor' application.
    getDataFieldFromFunctApp :: Name -> Type -> m DataField
    getDataFieldFromFunctApp nm' t = do
        assertInFunct nm'
        assertClassMembership nm' ''Traversable
        liftM (Funct nm') (getDataFieldFromType t)

    -- | Gets a 'DataField' from a 'Bifunctor' application.
    getDataFieldFromBifunctApp :: Name -> Type -> Type -> m DataField
    getDataFieldFromBifunctApp nm' t1 t2 = do
        assertInFunct nm'
        assertClassMembership nm' ''Bitraversable
        liftM2 (Bifunct nm') (getDataFieldFromType t1)
                             (getDataFieldFromType t2)

    -- | Gets a 'DataField' from a 'Trifunctor' application.
    getDataFieldFromTrifunctApp :: Name -> Type -> Type -> Type -> m DataField
    getDataFieldFromTrifunctApp nm' t1 t2 t3 = do
        assertInFunct nm'
        assertClassMembership nm' ''Tritraversable
        liftM3 (Trifunct nm') (getDataFieldFromType t1)
                              (getDataFieldFromType t2)
                              (getDataFieldFromType t3)

    -- | If the 'Name' is of a type synonym, returns the type it
    -- defines, else 'Nothing'
    getTypeSynDef :: Name -> m (Maybe (Name, Type))
    getTypeSynDef nm' = do
        info <- reifyTypeName nm'
        case info of
            TyConI dec -> case dec of
                TySynD nm tvbs t -> withStackTop nm $ if null tvbs
                    then return $ Just (nm, t)
                    else throwErrWithStack $ ErrParamTypeSyn (pretty dec)
                _ -> return Nothing

            -- TODO Or should this be an error?
            _ -> return Nothing

    -- | Assert that the 'Name' is declared as a functor.
    assertInFunct :: Name -> m ()
    assertInFunct nm' = unless (nm' `elem` functs)
        $ throwErrWithStack $ ErrNoFunct nm'

    -- | Assert that the 'Name' is declared as a member of the class.
    assertClassMembership :: Name -> Name -> m ()
    assertClassMembership nm' clsNm
        | (nm', clsNm) == (''[], ''Traversable)         = return ()
        | (nm', clsNm) == (''(,), ''Bitraversable)      = return ()
        | (nm', clsNm) == (''(,,), ''Tritraversable)    = return ()
        | otherwise                                     = do
            info <- reifyTypeName clsNm
            case info of
                ClassI _dec instances -> do
                    noInst <- anyM (matchingInst info) instances
                    unless noInst $ throwErrWithStack $ ErrNoInstance clsNm nm'
                _ -> throwErrWithStack $ ErrNoClass clsNm

        where
        matchingInst :: Info -> InstanceDec -> m Bool
        matchingInst info dec = case dec of
            InstanceD _ (AppT _ (ConT nm'')) _ -> return $ nm' == nm''
            DataInstD _ nm'' [] _ _ -> return $ nm' == nm''
            NewtypeInstD {} -> throwErrWithStack
                                   $ ErrReifyUnimpl nm' "NewtypeInstD" info
            TySynInstD {} -> throwErrWithStack
                                 $ ErrReifyUnimpl nm' "TySynInstD" info
            _ -> return False

        anyM :: (a -> m Bool) -> [a] -> m Bool
        anyM _ [] = return False
        anyM p (a : as) = do
            b <- p a
            if b
                then return True
                else anyM p as

----------------
-- BuildErr
----------------

-- | A stack of 'Name's being processed
type Stack = [Name]

-- | The pretty-printing of a 'Doc' of a 'Dec'.
-- 'Language.Haskell.TH.PprLib.Doc' is not in 'Data', so we have to
-- convert to a String to allow it to be spliced.
type DecDoc = String

{-

TODO There's a bug in 'dataToExpQ' when applied to 'Name's: those with
'NameFlavour' 'NameU' get confused when you try to splice them back,
since the 'NameU' is supposed to be unique and we're creating them by
parts instead of calling 'newName': this breaks an invariant.  The
symptom is an error reading:

Kind incompatibility when matching types:
      a0 :: *
      ghc-prim:GHC.Prim.Int# :: #
    Expected type: Integer -> ghc-prim:GHC.Prim.Int#
      Actual type: Integer -> a0
    In the first argument of ‘Language.Haskell.TH.Syntax.NameU’, namely
      ‘1761625784’
    In the second argument of ‘Language.Haskell.TH.Syntax.Name’, namely
      ‘Language.Haskell.TH.Syntax.NameU 1761625784’
    In the first argument of ‘PlainTV’, namely
      ‘Language.Haskell.TH.Syntax.Name
         (Language.Haskell.TH.Syntax.OccName "a")
         (Language.Haskell.TH.Syntax.NameU 1761625784)’

This comes up when we have 'Dec's in 'BuildErr's that have parameters.
When we splice the 'BuildErr' with 'buildFoldFamilyMaybe', the 'Dec'
contains unique names, which cause the problems.  We'll bypass this by
encoding the 'pretty' of the 'Dec' instead, the 'DecDoc', since we
only use the value for output in diagnostics.

-}

-- | Errors possible while building a 'FoldFamily'
data BuildErr = ErrDupCtors (S.Set String)
    | ErrEmptyData Name Stack
    | ErrEmptyFold
    | ErrInfixCtor Name Stack
    | ErrMonadFail String
    | ErrNoClass Name Stack
    | ErrNoCtor Name Stack
    | ErrNoFunct Name Stack
    | ErrNoInstance Name Name Stack
    | ErrParamType DecDoc Stack
    | ErrParamTypeSyn DecDoc Stack
    | ErrReify Name Info Stack
    | ErrReify' Name DecDoc Stack
    | ErrReifyUnimpl Name String Info Stack
    | ErrThrownInQ
    | ErrUnimpl String Stack
    | ErrUnsupported String Stack
    deriving (D.Data, D.Typeable)

instance Show BuildErr where
    show (ErrDupCtors ctors) = concat [
        "Different types use the same constructor name(s): ",
        intercalate ", " (map show $ S.toList ctors),
        "."]
    show (ErrEmptyData nm stk)
        = showStk stk (pretty nm ++ " has no constructors.")
    show ErrEmptyFold
        = "No constructors are used. The resulting fold would be empty."
    show (ErrInfixCtor nm stk)
        = showStk stk
              $ concat ["Infix constructors like (", pretty nm, ") are not yet supported."]
    show (ErrMonadFail msg) = msg
    show (ErrNoClass nm stk)
        = showStk stk
              $ concat ["Class ", pretty nm, " is not visible at the splice."]
    show (ErrNoCtor nm stk)
        = showStk stk
              $ concat ["Constructor ",
                        pretty nm,
                        " is not visible at the splice."]
    show (ErrNoFunct nm stk)
        = showStk stk $ concat ["Type ",
                                pretty nm,
                                " is used in functor position but",
                                " is not declared in the splice."]
    show (ErrNoInstance cls nm stk)
        = showStk stk $ concat ["There is no instance of ",
                                pretty cls,
                                " ",
                                pretty nm,
                                " visible at the splice."]
    show (ErrParamType decDoc stk)
        = showStk stk $ concat [decDoc,
                                " has parameters, ",
                                "which is not yet supported."]
    show (ErrParamTypeSyn decDoc stk)
        = showStk stk $ concat [decDoc,
                                " has parameters, ",
                                "which is not yet supported."]
    show (ErrReify nm info stk)
        = showStk stk $ concat ["reify ",
                                pretty nm,
                                "returned non-type Info: ",
                                pretty info,
                                "."]
    show (ErrReify' nm decDoc stk)
        = showStk stk $ concat ["reify ",
                                pretty nm,
                                "returned Info with bad declaration: ",
                                decDoc,
                                "."]
    show (ErrReifyUnimpl nm tag info stk)
        = showStk stk $ concat ["Not handling Decs of type ",
                                tag,
                                " while looking for instances for ",
                                pretty nm,
                                " in ",
                                pretty info,
                                "."]
    show ErrThrownInQ = "Unknown error thrown in Q monad."
    show (ErrUnimpl msg stk) = showStk stk (msg ++ " unimplemented.")
    show (ErrUnsupported msg stk) = showStk stk (msg ++ " not yet supported.")

-- | Prepends the stack trace
showStk :: Stack -> String -> String
showStk stk msg = concat ["Error while processing ",
                          intercalate " <= " $ map (show . pretty) stk,
                          ":\n",
                          msg]

----------------
-- Data
----------------

-- | Type synonym isomorphic to 'DataTy'
type Data = (Name, [DataCase])

-- | Turn a list of 'Data' into a 'FoldFamily', catching errors.
processData :: [Data] -> Either BuildErr FoldFamily
processData data' = do
    when (null data') $ Left ErrEmptyFold

    let ff = FoldFamily [DataTy ws dcs
                             | (ws, dcs) <- M.toList $ M.fromList data']

    let dupCtors = duplicateCtorNames ff

        -- TODO A better error would tell you both the types and the
        -- ctors.

    unless (S.null dupCtors) $ Left $ ErrDupCtors dupCtors

    return ff

----------------
-- MonadBuild class
----------------

-- | Monads for building 'FoldFamily's
class (Functor m, Applicative m, Monad m) => MonadBuild m where
    getData :: m a -> m (a, [Data])
    getStack :: m Stack
    putDataTy :: Name -> [DataCase] -> m ()
    reifyTypeName :: Name -> m Info
    see :: Name -> m ()
    seen :: Name -> m Bool
    throwErr :: BuildErr -> m a
    withStackTop :: Name -> m a -> m a

-- | Throws a 'BuildErr' with the current stack
throwErrWithStack :: MonadBuild m => (Stack -> BuildErr) -> m a
throwErrWithStack err = do
    stk <- getStack
    throwErr $ err stk

----------------
-- Concrete monad 'Build'
----------------

-- | A concrete instance of 'MonadBuild'
newtype Build a = Build {
    unB :: ErrorT BuildErr (RWST Stack [Data] (S.Set Name) Q) a
    }
    deriving (Functor, Applicative, Monad,
              MonadError BuildErr,
              MonadReader Stack,
              MonadState (S.Set Name),
              MonadWriter [Data])

instance Error BuildErr where
    strMsg = ErrMonadFail

instance MonadBuild Build where
    getData = listen
    getStack = ask
    putDataTy ws dcs = tell [(ws, dcs)]
    reifyTypeName = reifyTypeName'
    see = modify . S.insert
    seen = gets . S.member
    throwErr = throwError
    withStackTop ws = local (ws:)

-- | Evaluate 'Build' values down to the 'Q' monad
runBuild :: Build a -> Q (Either BuildErr a)
runBuild m = liftM fst $ evalRWST (runErrorT $ unB m) [] S.empty

-- | A wrapped version of 'reify' that works on 'WS's and rethrows
-- exceptions in 'Q' into 'Build'.
reifyTypeName' :: Name -> Build Info
reifyTypeName' nm = do

    -- I can't extract the value of exceptions thrown by reify; I
    -- only know when they happen. :-( So the best we can do is to
    -- give a generic message of failure and note what caused the
    -- error.

    let nm' = mkName $ upperName nm

    -- TODO Investigate this: we might not get the error handled at
    -- the right time.  Either fix it or explain why it's right.

    mInfo <- liftQ $ recover (return Nothing) (liftM Just $ reify nm')
    case mInfo of
        Nothing -> throwErrWithStack $ ErrNoCtor nm
        Just info -> return info

    where
    liftQ :: Q a -> Build a
    liftQ = Build . lift . lift

------------------------------------------------------------

pretty :: Ppr a => a -> String
pretty = show . ppr
