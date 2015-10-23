-- | Data structures representing a set of datatypes to be folded.
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Origami.Internal.FoldFamily(
    -- * Data structures
    FoldFamily(..),
    DataTy(..),
    DataCase(..),
    DataField(..),
    Ty(..),
    -- * Lenses
    HasName(..),
    dataCases,
    dataFields,
    dataTys,
    -- * Prisms
    _Atomic,
    _Nonatomic,
    _Funct,
    _Bifunct,
    _Trifunct,
    ) where

import Control.Lens
import Data.Data
-- import qualified Data.Map as M
import Language.Haskell.TH

------------------------------------------------------------
-- data
------------------------------------------------------------

-- | Represents a set of datatypes to be folded.
newtype FoldFamily = FoldFamily [DataTy]
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Represents a datatype to be folded.
data DataTy = DataTy Name [DataCase]
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Represents one way to construct a datatype; that is, one of its
-- constructors and its arguments.
data DataCase = DataCase Name [DataField]
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Represents a component of a datatype; that is, an argument to one
-- of its constructors.
data DataField = Atomic Ty -- ^ a type to be taken verbatim, not to be folded
           | Nonatomic Ty  -- ^ a type to be recursively folded
           | Funct Name DataField
                 -- ^ an application of a 'Functor'
           | Bifunct Name DataField DataField
                 -- ^ an application of a 'Bifunctor'
           | Trifunct Name DataField DataField DataField
                 -- ^ an application of a 'Trifunctor'
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Represents a datatype's name.
newtype Ty = Ty Name
    deriving (Eq, Ord, Show, Data, Typeable)

------------------------------------------------------------
-- lenses
------------------------------------------------------------

-- | Access to the 'Name' of a Data structure
class HasName d where
    name :: Lens' d Name

-- | Access to the datatypes of a fold family.
dataTys :: Iso' FoldFamily [DataTy]
dataTys = iso (\ (FoldFamily dts) -> dts) FoldFamily

{-
-- | an 'Iso'' up to reordering
foldFamilyMap :: Iso' FoldFamily (M.Map Name [DataCase])
foldFamilyMap = iso r l
    where
    r (FoldFamily dts) = M.fromList [ (nm, dcs) | DataTy nm dcs <- dts ]
    l m = FoldFamily [ DataTy nm dcs | (nm, dcs) <- M.toList m]
-}

instance HasName DataTy where
    name = lens (\ (DataTy nm _) -> nm) (\ (DataTy _ dcs) nm -> DataTy nm dcs)

-- | Access to the 'DataCase's of a datatype
dataCases :: Lens' DataTy [DataCase]
dataCases = lens (\ (DataTy _ dcs) -> dcs)
                 (\ (DataTy nm _) dcs -> DataTy nm dcs)

instance HasName DataCase where
    name = lens (\ (DataCase nm _) -> nm)
                (\ (DataCase _ dfs) nm -> DataCase nm dfs)

-- | Access to the 'DataFields's of a 'DataCase'
dataFields :: Lens' DataCase [DataField]
dataFields = lens (\ (DataCase _ dfs) -> dfs)
                  (\ (DataCase nm _) dfs -> DataCase nm dfs)

-- | Provides a 'Traversal' for an atomic 'Ty' in a 'DataField'
_Atomic :: Prism' DataField Ty
_Atomic = prism Atomic (\ df -> case df of
                                    Atomic ty -> Right ty
                                    _ -> Left df)

-- | Provides a 'Traversal' for an nonatomic 'Ty' in a 'DataField'
_Nonatomic :: Prism' DataField Ty
_Nonatomic = prism Nonatomic (\ df -> case df of
                                          Nonatomic ty -> Right ty
                                          _ -> Left df)

-- | Provides a 'Traversal' for a 'Functor' application in a 'DataField'
_Funct :: Prism' DataField (Name, DataField)
_Funct = prism (uncurry Funct)
               ( \ df -> case df of
                             Funct nm df' -> Right (nm, df')
                             _ -> Left df)

-- | Provides a 'Traversal' for a 'Bifunctor' application in a 'DataField'
_Bifunct :: Prism' DataField (Name, DataField, DataField)
_Bifunct = prism (\ (nm, df, df') -> Bifunct nm df df')
                 (\ df -> case df of
                              Bifunct nm df' df'' -> Right (nm, df', df'')
                              _ -> Left df)

-- | Provides a 'Traversal' for a 'Trifunctor' application in a 'DataField'
_Trifunct :: Prism' DataField (Name, DataField, DataField, DataField)
_Trifunct = prism (\ (nm, df, df', df'') -> Trifunct nm df df' df'')
                  (\ df -> case df of
                               Trifunct nm df' df'' df'''
                                   -> Right (nm, df', df'', df''')
                               _ -> Left df)

instance HasName Ty where
    name = lens (\ (Ty nm) -> nm) (\ (Ty _) nm ->Ty nm)
