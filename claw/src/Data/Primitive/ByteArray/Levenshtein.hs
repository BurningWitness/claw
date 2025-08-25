{-# LANGUAGE BangPatterns #-}

{- | It's the algorithm from
 -   https://en.wikipedia.org/w/index.php?title=Levenshtein_distance&oldid=1220200383#Iterative_with_two_matrix_rows
 -}

module Data.Primitive.ByteArray.Levenshtein
  ( initialize
  , Data.Primitive.ByteArray.Levenshtein.iterate
  , rate
  ) where

import           Control.Monad.ST

import           Data.ByteString.Short (ShortByteString (..))
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Word



-- | Creates the initial row.
initialize
  :: ShortText            -- ^ Horizontal array.
  -> ST x (PrimArray Int)
initialize txt = do
  let ShortByteString arr = ST.toShortByteString txt

      len = sizeofByteArray arr

  v <- newPrimArray (len + 1)

  let fill i
        | i > len  = unsafeFreezePrimArray v
        | otherwise = do
            writePrimArray v i i
            fill (i + 1)

  fill 0



-- | Runs the iterations for a vertical array slice.
iterate
  :: Int                    -- ^ Offset into the vertical slice.
  -> ByteArray              -- ^ Vertical slice.
  -> ShortText              -- ^ Horizontal array.
  -> MutablePrimArray x Int -- ^ Mutable copy of the previous row.
  -> ST x (PrimArray Int)
iterate posA arr txt v0 = do
  let ShortByteString brr = ST.toShortByteString txt

      m = sizeofByteArray arr
      n = sizeofByteArray brr

  v1 <- newPrimArray (n + 1)

  let work i !r0 !r1
        | i >= m    = unsafeFreezePrimArray r0
        | otherwise = do
            writePrimArray r1 0 (posA + (i + 1))

            let form j
                  | j >= n    = pure ()
                  | otherwise = do
                      del <- readPrimArray r0 (j + 1)
                      ins <- readPrimArray r1 j
                      sub <- readPrimArray r0 j

                      let deletionCost  = del + 1
                          insertionCost = ins + 1

                          a = indexByteArray arr i :: Word8
                          b = indexByteArray brr j :: Word8

                          substitutionCost | a == b    = sub
                                           | otherwise = sub + 1

                      writePrimArray r1 (j + 1) $ min deletionCost
                                                    (min insertionCost substitutionCost)
                      form (j + 1)

            form 0

            work (i + 1) r1 r0

  work 0 v0 v1



-- | Retrieves the edit distance for the current point.
rate :: ShortText -> PrimArray Int -> Int
rate txt arr =
  let ShortByteString brr = ST.toShortByteString txt

      len = sizeofByteArray brr

  in indexPrimArray arr len
