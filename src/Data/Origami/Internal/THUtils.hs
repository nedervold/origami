-- | Convenience functions for working with Template Haskell syntax.
module Data.Origami.Internal.THUtils(
    appEs,
    appTs,
    funcT,
    funcTs,
    nms,
    unAppTs,
    upperName,
    varEs,
    varEs',
    varPs,
    varPs'
    ) where

import Data.Char(toUpper)
import Language.Haskell.TH.Syntax

-- | Repeated left-associative application of 'AppE'
appEs :: [Exp] -> Exp
appEs = foldl1 AppE

-- | Repeated left-associative application of 'AppT'
appTs :: [Type] -> Type
appTs = foldl1 AppT

-- | Application of '(->)'
funcT :: Type -> Type -> Type
funcT lhs rhs = appTs [ArrowT, lhs, rhs]

-- | Repeated right-associative application of '(->)'
funcTs :: [Type] -> Type
funcTs = foldr1 funcT

-- | An infinite list of names @x1@, @x2@...
nms :: [Name]
nms = map x [1..]
    where
    x :: Int -> Name
    x n = mkName ('x' : show n)

-- | An infinite list of names @x1'@, @x2'@...
nms' :: [Name]
nms' = map x' [1..]
    where
    x' :: Int -> Name
    x' n = mkName ('x' : show n ++ "'")

-- | An infinite list of expression variables named  @x1@, @x2@...
varEs :: [Exp]
varEs = map VarE nms

-- | An infinite list of expression variables named  @x1'@, @x2'@...
varEs' :: [Exp]
varEs' = map VarE nms'

-- | An infinite list of pattern variables named  @x1@, @x2@...
varPs :: [Pat]
varPs = map VarP nms

-- | An infinite list of pattern variables named  @x1'@, @x2'@...
varPs' :: [Pat]
varPs' = map VarP nms'

-- | Splits a 'Type' into the list of type applications that comprise
-- it.  @appTs . unAppTs == id@ (although @unAppTs . appTs@ may not
-- @== id@ if any of the arguments are applications themselves.
unAppTs :: Type -> [Type]
unAppTs = reverse . go
    where
    go (AppT t1 t2) = t2 : go t1
    go t = [t]

-- | Returns the 'nameBase' of the 'Name' with the first character
-- upper-cased
upperName :: Name -> String
upperName nm = toUpper c : cs
    where
    (c : cs) = nameBase nm
