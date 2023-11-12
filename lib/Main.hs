module Main where

import Test.QuickCheck (quickCheck)

prop_additionAssociative :: Double -> Double -> Double -> Bool
prop_additionAssociative x y z = x + (y + z) == (x + y) + z

main :: IO ()
main = quickCheck prop_additionAssociative
