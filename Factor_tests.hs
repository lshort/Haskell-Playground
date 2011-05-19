-- this is the test code for the module Factor.hs
-- usage:  Factor_tests x y 
-- where x is the first number you want to have factored
-- and y is the number of numbers to factor

import Factor
import Control.Monad.Par
import Data.List
import Test.QuickCheck
import System
import Debug.Trace

{-# LANGUAGE CPP #-}

{----------- Generating primes and testing for primeness  -------------}

sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, 0 /= x `mod` p]

primes :: Integral a => [a]
primes = sieve [2..]

-- some lists of primes in different data types
primesInteger = primes :: [Integer]
primesInt = primes :: [Int]

primeList :: [Integer]
primeList = (take 100 primesInteger) ++ primeList

getPrime x = primesInteger !! (x `mod` 100)


{------- building some lists of numbers -------}

build_assoc_list :: Integral a => [a] -> [a] -> [a]
build_assoc_list  xs ys =  nub $ sort [ a * b  | a <- xs, b <- ys ]

--  the ==> comes from Test.QuickCheck
prop_assoc x y   =   x > 0 ==>   y > 0 ==>   
      factors (x*y) == build_assoc_list (factors x) (factors y)
      
prop_prime x   =   x > 0 ==>
      let prime = getPrime x   in factors prime == [1,prime]

prop_simple x y   =   x > 0 ==>   y > 0 ==> 
      let primeX = getPrime x 
          primeY = getPrime y 
      in (factors $ primeX * primeY) == (sort $ nub [1,primeX,primeY,primeX*primeY])


{--------------  time to build the actual tests --------------}
testDepth = 6   -- actually N-1

check :: [Integer] -> Integer -> [Integer] -> Bool
check [] _ _  = True
check (x:xs) n ns = 
      let p = getPrime (1 + abs(fromIntegral x))
          ps = factors p
          p_list = build_assoc_list ns ps
      in  factors (n*p)  ==  p_list  &&  check xs (n*p) (p_list)

prop_complex z zs =  z > 0 ==>  
   (fromIntegral $ length zs) >= (fromIntegral (z `mod` testDepth)) ==>
      check (take (z `mod` testDepth) zs) 1 [1]

{------------------- the main test harness ------------------}

main = do 

  quickCheck prop_complex
  quickCheck prop_assoc

  args <-  getArgs
  let start = read $ args !! 0 
  let end = (start + (read $ args !! 1))

-- test the parallel version
  mapM putStrLn $ runPar $ getFactorStringRange' start end

--  test the single threaded version 
--  mapM putStrLn $ getFactorStringRange start end

