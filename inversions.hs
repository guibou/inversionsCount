{-# LANGUAGE NoImplicitPrelude, LambdaCase, FlexibleContexts, Strict, TypeFamilies, ExplicitForAll #-}

import Protolude

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as V2

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

      let go idx1 idx2 count i
            | i == len = return count
            | otherwise = do
                cond <- (return (idx1 < mid)) .&&. (return (idx2 == len) .||. ((buf `at` idx1) .<=. (a `at` idx2)))
                if cond
                  then do
                  write a i (buf `at` idx1)
                  go (idx1 + 1) idx2 (count + idx2 - mid) (i+1)
                  else do
                  write a i (a `at` idx2)
                  go idx1 (idx2 + 1) count (i+1)

      go 0 mid (counta + countb) 0

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
at v idx = V.unsafeRead v idx

(.<=.) ma mb = (<=) <$> ma <*> mb
(.||.) ma mb = ma >>= \case
  True -> return True
  False -> mb
(.&&.) ma mb = ma >>= \case
  True -> mb
  False -> return False

write a idx v = V.unsafeWrite a idx =<< v
