module Data.Origami.Internal.TestFiles.Param where

data ParamTy a b c d = ParamTy

type ParamTySyn a b c d = ParamTy a a a a

data PT = PT (ParamTy String String String String)

data PTS = PTS (ParamTySyn String Char Bool ())
