-- | Provides a pretty-printing fold
module Data.Origami.Internal.Pretty(prettyFold) where

import Data.Origami.Internal.Fold(Fold(..))
import Text.PrettyPrint

------------------------------------------------------------
-- pretty
------------------------------------------------------------

-- | Folds the 'FoldFamily' fold family into pretty-printing 'Doc's
prettyFold :: Fold Doc Doc Doc Doc Doc
prettyFold = Fold {
    mkFoldFamily = \ dts -> text "Family" $$ nest 4 (vcat dts),
    mkDataTy = \ nm dcs -> text "Ty" <+> text (show nm) <+> arrow
                               $$ nest 4 (vcat dcs),
    mkDataCase = \ nm dfs -> text "Ctor" <+> text (show nm) <+> arrow
                               $$ nest 4 (vcat dfs),
    mkAtomic = \ ty -> text "Atomic" <+> ty,
    mkNonatomic = \ ty -> text "Nonatomic" <+> ty,
    mkFunct = \ nm df -> hsep [text "Funct", text $ show nm, parens df],
    mkBifunct = \ nm df df'
                     -> hsep [text "Bifunct", text $ show nm, parens df,
                                                              parens df'],
    mkTrifunct = \ nm df df' df''
                     -> hsep [text "Trifunct", text $ show nm, parens df,
                                                               parens df',
                                                               parens df''],
    mkTy = text . show
    }
    where
    arrow = text "->"
