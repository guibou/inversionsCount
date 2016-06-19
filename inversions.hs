{-# LANGUAGE NoImplicitPrelude, LambdaCase, FlexibleContexts, Strict, TypeFamilies, ExplicitForAll #-}

import Protolude

import qualified Data.ByteString.Char8 as B

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as V2

import Data.Time.Clock

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
                cond <- (idx1 < mid) &&. ((idx2 == len) ||. ((V.unsafeRead buf idx1) .<=. (V.unsafeRead a idx2)))
                if cond
                  then do
                  V.unsafeWrite a i =<< V.unsafeRead buf idx1
                  go (idx1 + 1) idx2 (count + idx2 - mid) (i+1)
                  else do
                  V.unsafeWrite a i =<< V.unsafeRead a idx2
                  go idx1 (idx2 + 1) count (i+1)

      go 0 mid (counta + countb) 0

-- This equals the C code in performance
parse' :: IO [Int32]
parse' = do
  content <- B.getContents
  return $ map fromIntegral $ unfoldr (B.readInt . B.dropWhile (=='\n')) $ content

-- This beats the C code in performance, at the cost of a few uglyness ;)
parse :: IO [Int32]
parse = do
  content <- B.getContents
  return $ go content
 where
  go b = case B.uncons b of
      Nothing -> []
      Just ('\n',b) -> go b
      Just ('-',b) -> go'' 0 b
      Just (d,b) -> go' (fromIntegral (ord d - 48)) b
  go' v b = case B.uncons b of
      Nothing -> [v]
      Just ('\n',b) -> v : go b
      Just (d,b) -> go' (v*10 + fromIntegral (ord d - 48)) b
  go'' v b = case B.uncons b of
      Nothing -> [v]
      Just ('\n',b) -> v : go b
      Just (d,b) -> go' (v*10 - fromIntegral (ord d - 48)) b

main :: IO ()
main = do
    start <- getCurrentTime
    nums <- parse
    middle <- getCurrentTime
    print (count_inversion nums)
    end <- getCurrentTime

    print ("Parsing time", diffUTCTime middle start)
    print ("Compute time", diffUTCTime end middle)


-- DSL for ST, because else the syntax sucks ;)
(.<=.) ma mb = (<=) <$> ma <*> mb

(||.) True _ = return True
(||.) False mb = mb

(&&.) False _ = return False
(&&.) True mb = mb
