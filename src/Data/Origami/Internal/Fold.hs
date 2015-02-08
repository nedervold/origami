-- | Folds over the fold family rooted at 'FoldFamily'.
module Data.Origami.Internal.Fold(
    -- * The 'Fold' structure and useful instances
    Fold(..),
    idFold,
    errFold,
    monadicFold,
    -- * folding functions
    foldFoldFamily,
    foldDataTy,
    foldDataCase,
    foldDataField,
    foldTy
    ) where

import Control.Monad
import Data.Origami.Internal.FoldFamily
import Language.Haskell.TH

------------------------------------------------------------
-- folds
------------------------------------------------------------

-- | The fold.  Bundles up the functions used to replace constructors
-- in the fold family.
data Fold dataCase dataField dataTy foldFamily ty = Fold {
    mkAtomic :: ty -> dataField,
    mkBifunct :: Name -> dataField -> dataField -> dataField,
    mkDataCase :: Name -> [dataField] -> dataCase,
    mkDataTy :: Name -> [dataCase] -> dataTy,
    mkFoldFamily :: [dataTy] -> foldFamily,
    mkFunct :: Name -> dataField -> dataField,
    mkNonatomic :: ty -> dataField,
    mkTrifunct :: Name -> dataField -> dataField -> dataField -> dataField,
    mkTy :: Name -> ty
    }

-- | The identity 'Fold'.  Intended as a base to be modified.
idFold :: Fold DataCase DataField DataTy FoldFamily Ty
idFold = Fold {
    mkAtomic = Atomic,
    mkBifunct = Bifunct,
    mkDataCase = DataCase,
    mkDataTy = DataTy,
    mkFoldFamily = FoldFamily,
    mkFunct = Funct,
    mkNonatomic = Nonatomic,
    mkTrifunct = Trifunct,
    mkTy = Ty
    }

-- | The error 'Fold'.	Intended as a base to be modified.
errFold :: String -> Fold dataCase dataField dataTy foldFamily ty
errFold str = Fold {
    mkAtomic = err "mkAtomic",
    mkBifunct = err "mkBifunct",
    mkDataCase = err "mkDataCase",
    mkDataTy = err "mkDataTy",
    mkFoldFamily = err "mkFoldFamily",
    mkFunct = err "mkFunct",
    mkNonatomic = err "mkNonatomic",
    mkTrifunct = err "mkTrifunct",
    mkTy = err "mkTy"
    }
    where
    err tag = error (str ++ "." ++ tag)

-- | Using the constructors from the base 'Fold', folds monadically in
-- a bottom-up, left-to-right manner.
monadicFold :: Monad m
    => Fold dataCase dataField dataTy foldFamily ty
    -> Fold (m dataCase) (m dataField) (m dataTy) (m foldFamily) (m ty)
monadicFold f = Fold {
    mkAtomic = liftM (mkAtomic f),
    mkBifunct = liftM2 . mkBifunct f,
    mkDataCase = \ nm dfs -> do { dfs' <- sequence dfs;
				  return $ mkDataCase f nm dfs' },
    mkDataTy = \ nm dcs -> do { dcs' <- sequence dcs;
				return $ mkDataTy f nm dcs' },
    mkFoldFamily = \ dts -> do { dts' <- sequence dts;
				 return $ mkFoldFamily f dts' },
    mkFunct = liftM . mkFunct f,
    mkTrifunct = liftM3 . mkTrifunct f,
    mkNonatomic = liftM (mkNonatomic f),
    mkTy = return . mkTy f
    }

------------------------------------------------------------
-- folds
------------------------------------------------------------

-- | Monadically folds over a 'DataCase'
foldDataCase :: Fold dataCase dataField dataTy foldFamily ty
	      -> DataCase -> dataCase
foldDataCase f (DataCase nm dfs)
    = mkDataCase f nm (fmap (foldDataField f) dfs)

-- | Monadically folds over a 'DataField'
foldDataField :: Fold dataCase dataField dataTy foldFamily ty
	       -> DataField -> dataField
foldDataField f (Atomic ty) = mkAtomic f (foldTy f ty)
foldDataField f (Nonatomic ty) = mkNonatomic f (foldTy f ty)
foldDataField f (Funct nm df) = mkFunct f nm
					(foldDataField f df)
foldDataField f (Bifunct nm df df') = mkBifunct f nm
						(foldDataField f df)
						(foldDataField f df')
foldDataField f (Trifunct nm df df' df'') = mkTrifunct f nm
						       (foldDataField f df)
						       (foldDataField f df')
						       (foldDataField f df'')

-- | Monadically folds over a 'DataTy'
foldDataTy :: Fold dataCase dataField dataTy foldFamily ty
	    -> DataTy -> dataTy
foldDataTy f (DataTy nm dcs) = mkDataTy f nm (fmap (foldDataCase f) dcs)

-- | Monadically folds over a 'FoldFamily'
foldFoldFamily :: Fold dataCase dataField dataTy foldFamily ty
                -> FoldFamily -> foldFamily
foldFoldFamily f (FoldFamily dts) = mkFoldFamily f (fmap (foldDataTy f) dts)

-- | Monadically folds over a 'FoldTy'
foldTy :: Fold dataCase dataField dataTy foldFamily ty
        -> Ty -> ty
foldTy f(Ty nm) = mkTy f nm
