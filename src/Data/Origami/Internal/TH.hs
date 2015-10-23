-- | Creation of declarations from a 'FoldFamily'
{-# LANGUAGE TemplateHaskell #-}
module Data.Origami.Internal.TH(mkFoldDecs,
    ctorNamesAreUnique,
    duplicateCtorNames,
    typeNamesAreUnique) where

import Control.Lens hiding (Fold)
import Data.Bitraversable(Bitraversable(..))
import Data.Char(toLower)
import Data.Foldable(Foldable(foldMap))
import Data.List(sort, sortBy)
import qualified Data.Map as M
import Data.Ord(comparing)
import Data.Origami.Internal.Fold(Fold(..), errFold, foldFoldFamily)
import Data.Origami.Internal.FoldFamily
import Data.Origami.Internal.THUtils
import Data.Origami.Internal.Trifunctor(Trifunctor(..), Tritraversable(..))
import Data.Sequence.Lens(seqOf)
import qualified Data.Set as S
import Data.Set.Lens(setOf)
import Data.Traversable(sequence)
import Language.Haskell.TH.Syntax
import Prelude hiding (sequence)

typeNames :: Traversal' FoldFamily Name
typeNames = dataTys . traverse . name

dataCases' :: Traversal' FoldFamily DataCase
dataCases' = dataTys . traverse . dataCases . traverse

-- | A 'Traversal' over the constructor 'Name's of the 'FoldFamily'
ctorNames :: Traversal' FoldFamily Name
ctorNames = dataCases' . name

-- | Returns @True@ iff the 'Name's of the datatypes in the
-- 'FoldFamily' are all unique.
typeNamesAreUnique :: FoldFamily -> Bool
typeNamesAreUnique = areUnique . seqOf typeNames

-- | Returns the set of constructor 'Name's in the 'FoldFamily' that
-- are repeated.
duplicateCtorNames :: FoldFamily -> S.Set String
duplicateCtorNames = duplicates . fmap upperName . seqOf ctorNames

-- | Returns @True@ iff the constructor 'Name's of the 'FoldFamily'
-- are all unique.
ctorNamesAreUnique :: FoldFamily -> Bool
ctorNamesAreUnique = areUnique . seqOf ctorNames

duplicates :: (Ord a, Foldable f) => f a -> S.Set a
duplicates = M.keysSet
                 . M.filter (> (1 :: Int))
                 . M.fromListWith (+)
                 . foldMap (\x -> [(x, 1)])

areUnique :: (Ord a, Foldable f) => f a -> Bool
areUnique = S.null . duplicates

lowerTHName :: Name -> Name
lowerTHName = mkName . lowerName
    where
    lowerName :: Name -> String
    lowerName nm = toLower c : cs
        where
        (c : cs) = nameBase nm

upperTHName :: Name -> Name
upperTHName = mkName . upperName

thMkName :: Name -> Name
thMkName = mkName . thMkString

thMkString :: Name -> String
thMkString nm = "mk" ++ upperName nm

thFoldName :: Name -> Name
thFoldName = mkName . thFoldString

thFoldString :: Name -> String
thFoldString nm = "fold" ++ upperName nm

foldName :: Name
foldName = mkName "Fold"

typeNameList :: FoldFamily -> [Name]
typeNameList = S.toList . setOf typeNames

{-
ctorNameList :: FoldFamily -> [Name]
ctorNameList = S.toList . setOf ctorNames
-}

-- | Creates declarations for the
-- * @Fold@,
-- * @idFold@,
-- * @errFold@,
-- * @monadicFold@,
-- * and one @foldXxx@ function for each datatype @Xxx@ in the
-- 'FoldFamily'.
mkFoldDecs :: FoldFamily -> [Dec]
mkFoldDecs ff = mkFoldDec ff : mkIdFoldDecs ff ++ mkErrFoldDecs ff
                                               ++ mkMonadicFoldDecs ff
                                               ++ mkFoldFuncDecs ff

-- | Creates a declaration for the @Fold@.
mkFoldDec :: FoldFamily -> Dec
mkFoldDec ff = foldFoldFamily fold' ff
    where
    fold' :: Fold (Name, [Type]) Type [(Name, VarStrictType)] Dec Name
    fold' = Fold {
                mkFoldFamily = mkFoldFamily',
                mkDataTy = mkDataTy',
                mkDataCase = (,),
                mkTy = id,
                mkAtomic = ConT . upperTHName,
                mkNonatomic = VarT . lowerTHName,
                mkFunct = mkFunct',
                mkBifunct = mkBifunct',
                mkTrifunct = mkTrifunct'
            }

    mkFoldFamily' :: [[(Name, VarStrictType)]] -> Dec
    mkFoldFamily' dts = DataD [] foldName tvbs [con] []
        where
        tvbs :: [TyVarBndr]
        tvbs = map (PlainTV . lowerTHName) $ typeNameList ff

        con :: Con
        con = RecC foldName vsts

        vsts :: [VarStrictType]
        vsts = map snd $ sortBy (comparing fst) $ concat dts

    mkDataTy' :: Name -> [(Name, [Type])] -> [(Name, VarStrictType)]
    mkDataTy' ty dcs
        = [(ctor, (ctorNm, NotStrict, fldTy))
               | (ctor, fldTys) <- dcs,
                 let ctorNm = thMkName ctor,
                 let fldTy = funcTs (fldTys ++ [resTy])]
        where
        resTy = VarT $ lowerTHName ty

    mkFunct' :: Name -> Type -> Type
    mkFunct' nm
        | nm == ''[]            = AppT ListT
        | otherwise             = AppT (ConT nm)

    mkBifunct' :: Name -> Type -> Type -> Type
    mkBifunct' nm lhs rhs
        | nm == ''(,)     = appTs [TupleT 2, lhs, rhs]
        | otherwise       = appTs [ConT nm, lhs, rhs]

    mkTrifunct' :: Name -> Type -> Type -> Type -> Type
    mkTrifunct' nm l' m' r'
        | nm == ''(,,)   = appTs [TupleT 3, l', m', r']
        | otherwise      = appTs [ConT nm, l', m', r']

-- | Creates a declaration for the @idFold@.
mkIdFoldDecs :: FoldFamily -> [Dec]
mkIdFoldDecs ff = foldFoldFamily fold' ff
    where
    fold' :: Fold Name dataField [Name] [Dec] ty
    fold' = (errFold "mkIdFoldDecs.fold'"){
                mkFoldFamily = mkFoldFamily',
                mkDataTy = mkDataTy',
                mkDataCase = const
            }

    mkFoldFamily' :: [[Name]] -> [Dec]
    mkFoldFamily' dcs = [SigD nm ty, ValD pat bd []]
        where
        nm :: Name
        nm = mkName "idFold"

        ty :: Type
        ty = appTs $ map ConT $ foldName : map upperTHName (typeNameList ff)

        pat :: Pat
        pat = VarP nm

        bd :: Body
        bd = NormalB
                 $ RecConE foldName
                       [(thMkName ws, ConE $ upperTHName ws) | ws <- ctors ]

        ctors :: [Name]
        ctors = sort $ concat dcs

    mkDataTy' :: Name -> [Name] -> [Name]
    mkDataTy' _ = id

foldTy' :: FoldFamily -> Type
foldTy' ff = appTs $ ConT foldName : map (VarT. lowerTHName) tyNms
    where
    tyNms = typeNameList ff

-- | Creates a declaration for the @errFold@.
mkErrFoldDecs :: FoldFamily -> [Dec]
mkErrFoldDecs ff = foldFoldFamily fold' ff
    where
    fold' :: Fold (Name, FieldExp) dataField [(Name, FieldExp)] [Dec] ty
    fold' = (errFold "errFoldDecs"){
                mkFoldFamily = mkFoldFamily',
                mkDataTy = flip const,
                mkDataCase = mkDataCase'
            }

    mkFoldFamily' :: [[(Name, FieldExp)]] -> [Dec]
    mkFoldFamily' dts = [SigD nm ty, FunD nm [cl]]
        where
        cl :: Clause
        cl = Clause [VarP foldTagNm] bd [errDef]

        bd :: Body
        bd = mkSortedRecBody foldName dts

    mkDataCase' :: Name -> [dataField] -> (Name, FieldExp)
    mkDataCase' ctor _ = (ctor, (thMkName ctor, errExp))
        where
        errExp :: Exp
        errExp = AppE (VarE errNm) (LitE $ StringL $ thMkString ctor)

    foldTagNm :: Name
    foldTagNm = mkName "foldTag'"

    nm :: Name
    nm = mkName "errFold"

    ty :: Type
    ty = funcT (ConT ''String) (ForallT tvbs [] $ foldTy' ff)
        where
        tvbs :: [TyVarBndr]
        tvbs = map (PlainTV. lowerTHName) $ typeNameList ff

    errNm :: Name
    errNm = mkName "err"

    errDef :: Dec
    errDef = FunD errNm [cl']
        where
        fieldTagNm :: Name
        fieldTagNm = mkName "fieldTag"

        cl' :: Clause
        cl' = Clause [VarP fieldTagNm] bd' []

        bd' :: Body
        bd' = NormalB $ AppE (VarE $ mkName "error")
                             (ParensE $ AppE (VarE $ mkName "concat") $
                                              ListE [VarE foldTagNm,
                                                     LitE $ StringL ".",
                                                     VarE fieldTagNm])

-- | Creates a declaration for the @monadicFold@.
mkMonadicFoldDecs :: FoldFamily -> [Dec]
mkMonadicFoldDecs ff = foldFoldFamily fold' ff
    where
    fold' :: Fold (Name, FieldExp) Exp [(Name, FieldExp)] [Dec] ty
    fold' = Fold {
                mkFoldFamily = mkFoldFamily',
                mkDataTy = const id,
                mkDataCase = mkDataCase',
                mkAtomic = mkAtomic',
                mkNonatomic = mkNonatomic',
                mkFunct = mkFunct',
                mkBifunct = mkBifunct',
                mkTrifunct = mkTrifunct',
                mkTy = error "mkMonadicFoldDecs.mkTy"
            }

    nm :: Name
    nm = mkName "monadicFold"

    m :: Name -> Type
    m nm' = AppT (VarT mNm) (VarT nm')

    mNm :: Name
    mNm = mkName "m"

    monadicFoldTy :: Type
    monadicFoldTy = appTs (ConT foldName
                              : map (m . lowerTHName) (typeNameList ff))

    baseFoldName :: Name
    baseFoldName = mkName "baseFold"

    mkFoldFamily' :: [[(Name, FieldExp)]] -> [Dec]
    mkFoldFamily' dcs = [SigD nm ty',
                         FunD nm [cl]]
        where
        ty' :: Type
        ty' = ForallT tvbs [] ty

        tvbs :: [TyVarBndr]
        tvbs = map (PlainTV . lowerTHName) $ typeNameList ff

        ty :: Type
        ty = funcT baseFoldTy
                   (ForallT [PlainTV mNm]
                            [ClassP ''Monad [VarT mNm]]
                            monadicFoldTy)

        baseFoldTy :: Type
        baseFoldTy = foldTy' ff

        cl :: Clause
        cl = Clause [VarP baseFoldName] bd []

        bd :: Body
        bd = mkSortedRecBody foldName dcs

    mkDataCase' :: Name -> [Exp] -> (Name, FieldExp)
    mkDataCase' ctor dfs = (ctor, (thMkName ctor, LamE pats doE))
        where
        pats :: [Pat]
        pats = zipWith const varPs dfs

        doE :: Exp
        doE = DoE (bindStmts ++ [NoBindS resE])

        bindStmts :: [Stmt]
        bindStmts = [BindS p (AppE df e) | (p, df, e) <- zip3 varPs' dfs varEs]

        resE :: Exp
        resE = AppE (VarE 'return)
                    (appEs $ VarE (thMkName ctor) : VarE baseFoldName : exps')
            where
            exps' :: [Exp]
            exps' = zipWith const varEs' dfs

    mkAtomic' :: ty -> Exp
    mkAtomic' _ = VarE 'return

    mkNonatomic' :: ty -> Exp
    mkNonatomic' _  = VarE 'id

    mkFunct' :: Name -> Exp -> Exp
    mkFunct' _ f = ParensE $ comp (VarE 'sequence)
                                  (appEs [VarE 'fmap, f])

    mkBifunct' :: Name -> Exp -> Exp -> Exp
    mkBifunct' _ l r = ParensE $ comp (VarE 'bisequence)
                                      (appEs [VarE 'bimap, l, r])

    mkTrifunct' :: Name -> Exp -> Exp -> Exp -> Exp
    mkTrifunct' _ l' m' r' = ParensE $ comp (VarE 'trisequence)
                                            (appEs [VarE 'trimap, l', m', r'])

    -- | Composes two 'Exp's.
    comp :: Exp -> Exp -> Exp
    comp lhs rhs = InfixE (Just lhs) composeE (Just rhs)
        where
        composeE :: Exp
        composeE = VarE $ mkName "."

-- | Creates a fold function @foldXxx@ for each datatype @Xxx@ in the
-- 'FoldFamily'.
mkFoldFuncDecs :: FoldFamily -> [Dec]
mkFoldFuncDecs ff = foldFoldFamily fold' ff
    where
    fold' :: Fold Clause Exp [Dec] [Dec] Name
    fold' = Fold {
                mkFoldFamily = concat,
                mkDataTy = mkDataTy',
                mkDataCase = mkDataCase',
                mkAtomic = const (VarE 'id),
                mkNonatomic = mkNonatomic',
                mkFunct = mkFunct',
                mkBifunct = mkBifunct',
                mkTrifunct = mkTrifunct',
                mkTy = id
            }

    fName :: Name
    fName = mkName "f"

    fExp :: Exp
    fExp = VarE fName

    fPat :: Pat
    fPat = VarP fName

    mkDataTy' :: Name -> [Clause] -> [Dec]
    mkDataTy' nm dcs = [SigD foldNm ty, FunD foldNm dcs]
        where
        foldNm :: Name
        foldNm = thFoldName nm

        ty :: Type
        ty = ForallT tvbs [] ty'
            where
            tvbs :: [TyVarBndr]
            tvbs = map (PlainTV. lowerTHName) tyNms

            tyNms :: [Name]
            tyNms = typeNameList ff

            ty' :: Type
            ty' = funcTs [foldTy' ff,
                          ConT $ upperTHName nm,
                          VarT $ lowerTHName nm]

    mkDataCase' :: Name -> [Exp] -> Clause
    mkDataCase' ctor dfs = Clause [fPat, argPat] bd []
        where
        argPat :: Pat
        argPat = ConP (upperTHName ctor) $ zipWith const varPs dfs

        bd :: Body
        bd = NormalB $ appEs $ (VarE $ thMkName ctor)
                               : fExp
                               : zipWith AppE dfs varEs

    mkNonatomic' :: Name -> Exp
    mkNonatomic' ws = AppE (VarE $ thFoldName ws) fExp

    mkFunct' :: Name -> Exp -> Exp
    mkFunct' _ = AppE (VarE 'fmap)

    mkBifunct' :: Name -> Exp -> Exp -> Exp
    mkBifunct' _ e e' = appEs [VarE 'bimap, e, e']

    mkTrifunct' :: Name -> Exp -> Exp -> Exp -> Exp
    mkTrifunct' _ e e' e'' = appEs [VarE 'trimap, e, e', e'']

mkSortedRecBody :: Name -> [[(Name, FieldExp)]] -> Body
mkSortedRecBody nm taggedFieldExps = NormalB $ RecConE nm fieldExps
    where
    fieldExps :: [FieldExp]
    fieldExps = map snd $ sortBy (comparing fst) $ concat taggedFieldExps
