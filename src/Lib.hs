{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.Void (Void)
import GHC.Base (Nat)
import GHC.Generics
  ( Generic (Rep, from),
    K1 (K1),
    M1 (M1),
    U1,
    V1,
    type (:*:) (..),
    type (:+:) (..),
  )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- hash to an int
class Hash a where
  hash :: a -> Int
  default hash :: (Generic a, Hash1 (Rep a)) => a -> Int
  hash = hash1 . from

-- * -> * class for generics
class Hash1 f where
  hash1 :: f p -> Int

-- void
instance Hash1 V1 where
  hash1 a = case a of

-- unit
instance Hash1 U1 where
  hash1 _ = 1

-- value
instance (Hash c) => Hash1 (K1 i c) where
  hash1 (K1 c) = hash c

-- union
instance (Hash1 a, Hash1 b) => Hash1 (a :+: b) where
  hash1 (L1 a) = 11 * hash1 a
  hash1 (R1 b) = 13 * hash1 b

-- tuple
instance (Hash1 a, Hash1 b) => Hash1 (a :*: b) where
  hash1 (a :*: b) = 17 * hash1 a + 19 * hash1 b

-- meta wrapper
instance (Hash1 f) => Hash1 (M1 i t f) where
  hash1 (M1 x) = hash1 x

primes :: [Int]
primes = go [2 ..]
  where
    go (x : xs) = x : filter (\y -> y `mod` x /= 0) xs
    go [] = []

primes' :: [Int]
primes' = drop 5 primes

combineHashes :: [Int] -> Int
combineHashes hashes = sum (zipWith (*) primes' hashes)

hashFoldable :: (Foldable f, Hash a) => f a -> Int
hashFoldable = combineHashes . fmap hash . toList

hashIntegral :: Integral a => a -> Int
hashIntegral = fromIntegral

hashEnum :: Enum a => a -> Int
hashEnum = fromEnum

hashEq :: Hash a => a -> a -> Bool
hashEq = (==) `on` hash

instance Hash Int where
  hash = id

instance Hash Integer where
  hash = hashIntegral

instance Hash a => Hash [a] where
  hash = combineHashes . fmap hash

instance Hash a => Hash (Maybe a)

instance Hash Bool

instance (Hash a, Hash b) => Hash (Either a b)

instance Hash Void

instance Hash ()

instance (Hash a, Hash b) => Hash (a, b)

instance (Hash a, Hash b, Hash c) => Hash (a, b, c)

instance (Hash a, Hash b, Hash c, Hash d) => Hash (a, b, c, d)

instance (Hash a, Hash b, Hash c, Hash d, Hash e) => Hash (a, b, c, d, e)

instance Hash Char where
  hash = hashEnum
