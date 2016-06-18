import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed as V2

import Data.Int (Int32)
import Data.IORef

import Data.Foldable (for_)

count_inversion :: V.IOVector Int32 -> IO Int
count_inversion d = do
  aux_buf <- V.replicate (V.length d `div` 2) 0
  count_inv d aux_buf

count_inv :: V.IOVector Int32
          -> V.IOVector Int32
          -> IO Int
count_inv a buf
  | V.length a <= 1 = return 0
  | otherwise = do
      let len = V.length a
          mid = len `div` 2

      counta <- count_inv (V.slice 0 mid a) buf
      countb <- count_inv (V.slice mid (len - mid) a) buf

      V.unsafeCopy (V.slice 0 mid buf) (V.slice 0 mid a)

      idx1' <- newIORef 0
      idx2' <- newIORef mid

      count <- newIORef (counta + countb)

      for_ [0..(len - 1)] $ \i -> do
        idx1 <- readIORef idx1'
        idx2 <- readIORef idx2'

        let okBranch = do
             V.unsafeWrite a i =<< V.unsafeRead buf idx1
             modifyIORef' idx1' (+1)
             modifyIORef' count (+(idx2 - mid))

        let wrongBranch = do
             V.unsafeWrite a i =<< V.unsafeRead a idx2
             modifyIORef' idx2' (+1)

        if idx1 < mid then if (idx2 == len)
                           then okBranch
                           else do
                               buf_idx1 <- V.unsafeRead buf idx1
                               a_idx2 <- V.unsafeRead a idx2
                               if buf_idx1 <= a_idx2
                                 then okBranch
                                 else wrongBranch
          else wrongBranch

      readIORef count

parse :: IO [Int32]
parse = do
  content <- Data.ByteString.getContents
  let Right res = P.parseOnly (P.decimal `P.sepBy` (P.char '\n') ) content
  return res

main :: IO ()
main = do
    nums <- parse
    v <- V2.unsafeThaw ((V2.fromList nums))
    c <- count_inversion v
    print c
