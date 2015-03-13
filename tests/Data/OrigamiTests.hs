module Data.OrigamiTests (tests) where

import qualified Data.Origami.Internal.BuildTests(tests)
import Test.Framework(Test, testGroup)
-- import Test.Framework.Providers.HUnit(testCase)
-- import Test.HUnit(assertEqual)

tests :: Test
tests = testGroup "Data.Origami" [ Data.Origami.Internal.BuildTests.tests ]
