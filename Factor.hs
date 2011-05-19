-- see Factor_tests.hs for the test code that uses this API

module Factor( getFactorStringRange, getFactorStringRange', factors, factors' ) where

import Control.Monad.Par
import Data.List
import Test.QuickCheck
import System
import Debug.Trace

factors :: Integer -> [Integer]
factors x
  | x < 2      = [1]
  | otherwise  = sort $ factors' x 2 [1,x]

factors' :: Integer -> Integer -> [Integer] -> [Integer]
factors' x y zs 
  | fromInteger y > (sqrt $ fromInteger x)  = zs
  | x `mod` y == 0  &&  y*y /= x            = factors' x (y+1) (y : x `div` y : zs)
  | x `mod` y == 0                          = factors' x (y+1) (y : zs)
  | otherwise                               = factors' x (y+1) zs

getFactorString :: Integer -> String
getFactorString x = concat $ intersperse "," $ map show $ factors x

getFactorStringRange :: Integer -> Integer -> [String]
getFactorStringRange x y = map getFactorString [x..y]

getFactorString' :: Integer -> Par String
getFactorString' x = do
    calc <- spawn $ return $ concat $ intersperse "," $ map show $ factors x
    v <- get calc
    return v

getFactorStringRange' :: Integer -> Integer -> Par [String]
getFactorStringRange' x y = parMapM getFactorString' [x..y]

