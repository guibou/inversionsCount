{-# LANGUAGE NoImplicitPrelude, LambdaCase, FlexibleContexts, Strict, TypeFamilies, ExplicitForAll #-}

import Protolude

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as V2

import Data.Mutable

count_inversion :: [Int32] -> Int
count_inversion l = runST $ do
  d <- V2.unsafeThaw (V2.fromList l)
  aux_buf <- V.replicate (V.length d `div` 2) 0
  count_inv d aux_buf

count_inv a buf
  | V.length a <= 1 = return 0
  | otherwise = do
      let len = V.length a
          mid = len `div` 2

      counta <- count_inv (V.slice 0 mid a) buf
      countb <- count_inv (V.slice mid (len - mid) a) buf

      V.unsafeCopy (V.slice 0 mid buf) (V.slice 0 mid a)

      idx1 <- mutable 0
      idx2 <- mutable mid
      count <- mutable (counta + countb)

      for_ [0..(len - 1)] $ \i -> do
        cond <- (idx1 .< mid) .&&. ((idx2 .== len) .||. ((buf `at` idx1) .<=. (a `at` idx2)))
        if cond
          then do
             write a i (buf `at` idx1)
             inc idx1
             count += (idx2 .- mid)
          else do
             write a i (a `at` idx2)
             inc idx2

      readRef count

parse :: IO [Int32]
parse = do
  content <- Data.ByteString.getContents
  let Right res = P.parseOnly (P.decimal `P.sepBy` (P.char '\n') ) content
  return res

main :: IO ()
main = do
    nums <- parse
    print (count_inversion nums)


-- DSL for ST, because else the syntax sucks ;)
(.-) ref value = readRef ref >>= (\v -> return (v - value))
(+=) v value = value >>= \v' -> modifyRef' v (+v')
inc v = modifyRef' v (+1)
at v idx = V.unsafeRead v =<< readRef idx

(.<) a b = (<b) <$> readRef a
(.==) a b = (==b) <$> readRef a
(.<=.) ma mb = (<=) <$> ma <*> mb
(.||.) ma mb = ma >>= \case
  True -> return True
  False -> mb
(.&&.) ma mb = ma >>= \case
  True -> mb
  False -> return False

write a idx v = V.unsafeWrite a idx =<< v

-- This signature is mandatory for good performances
-- I still don't understand why
mutable :: forall s. Int -> ST s (PRef s Int)
mutable v = asPRef <$> newRef v
