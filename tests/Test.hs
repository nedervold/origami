module Main where

import Data.OrigamiTests(tests)
import Test.Framework(defaultMain)

main :: IO ()
main = defaultMain [tests]


