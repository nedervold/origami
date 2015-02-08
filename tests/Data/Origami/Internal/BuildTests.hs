{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Origami.Internal.BuildTests(tests) where

import Data.Origami.Internal.Build(BuildErr, buildFoldFamilyMaybe)
import Data.Origami.Internal.FoldFamily(FoldFamily)
import Data.Origami.Internal.TestFiles.Empty
import Data.Origami.Internal.TestFiles.Foo
import Data.Origami.Internal.TestFiles.Infix
import Data.Origami.Internal.TestFiles.Param
import qualified Data.Origami.Internal.TestFiles.Qual
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(Assertion, assertFailure)

-- | Tests that errors are caught in the build process, before
-- splicing.  The design philosophy here is that users should get
-- meaningful (to them) error messages, rather than obscure errors at
-- splice time.
--
-- Although I'm testing the 'BuildErr's, I'm not doing testing the
-- 'FoldFamily's, since when they are wrong, they are usually
-- catastrophically wrong and problems will be immediately noticed.
-- (This decision may need to be revisited.)
tests :: Test
tests = testGroup "BuildTests" [
    testFailsOnBuildingEmptyFold,
    testFailsOnIdenticallyNamedConstructors,
    testFailsOnParameterizedType,
    testFailsOnParameterizedTypeSynonym,
    testFailsOnInfixConstructor,
    testFailsOnQualifiedType,
    testFailsOnEmptyData
    ]

testFailsOnParameterizedType :: Test
testFailsOnParameterizedType
    = testCase msg
	  $ assertBuildFailed $(buildFoldFamilyMaybe [''PT] []
						     [''String])
    where
    msg = "testFailsOnParameterizedType"

testFailsOnParameterizedTypeSynonym :: Test
testFailsOnParameterizedTypeSynonym
    = testCase msg
	  $ assertBuildFailed $(buildFoldFamilyMaybe [''PTS] []
						     [''String])
    where
    msg = "testFailsOnParameterizedTypeSynonym"

testFailsOnEmptyData :: Test
testFailsOnEmptyData
    = testCase msg
	  $ assertBuildFailed $(buildFoldFamilyMaybe [''Empty ] [] [])
    where
    msg = "testFailsOnEmptyData"

testFailsOnInfixConstructor :: Test
testFailsOnInfixConstructor
    = testCase msg
	  $ assertBuildFailed $(buildFoldFamilyMaybe [''Infix ] [] [])
    where
    msg = "testFailsOnInfixConstructor"

testFailsOnQualifiedType :: Test
testFailsOnQualifiedType
    = testCase msg
	  $ assertBuildFailed
		$(buildFoldFamilyMaybe
		      [''Data.Origami.Internal.TestFiles.Qual.Qual ]
		      [] [])
    where
    msg = "testFailsOnQualifiedType"

testFailsOnIdenticallyNamedConstructors :: Test
testFailsOnIdenticallyNamedConstructors
    = testCase msg $ assertBuildFailed $(buildFoldFamilyMaybe [''Foo ] [] [])

    where
    msg = "testFailsOnIdenticallyNamedConstructors"

testFailsOnBuildingEmptyFold :: Test
testFailsOnBuildingEmptyFold
    = testCase msg
	  $ assertBuildFailed
		$(buildFoldFamilyMaybe [''Int] [] [''Int])
    where
    msg = "testFailsOnBuildingEmptyFold"

------------------------------------------------------------

assertBuildFailed :: Either BuildErr FoldFamily -> Assertion
assertBuildFailed e = case e of
    Left _ -> return ()
    Right _ff -> assertFailure "test didn't fail; was supposed to"
