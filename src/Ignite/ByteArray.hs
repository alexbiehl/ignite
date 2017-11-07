{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ignite.ByteArray where

import Ignite.Array

import Control.Monad.Primitive
import Data.Word (Word8)
import Foreign.Marshal.Utils
import Foreign.Ptr

type ByteArray = Array Word8

fill :: PrimMonad m => ByteArray -> Ptr Word8 -> Int -> m ()
fill array op n =
  arrayUnsafeWithElems array (\m elems -> unsafeIOToPrim (f m elems))
  where
    f :: Int -> Ptr Word8 -> IO ()
    f m elems = copyBytes elems op (min m n)
