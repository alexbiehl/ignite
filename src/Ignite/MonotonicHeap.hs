{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ignite.MonotonicHeap (
    Heap
  , withHeap
  , allocStruct
  , allocArray
  ) where

import Ignite.Layout
import Ignite.Array
import Ignite.Struct

import Control.Monad.Primitive
import Data.IORef
import Data.Primitive.ByteArray
import Data.Primitive.MutVar
import Data.Primitive.Types
import Data.Proxy
import Foreign.Ptr
import qualified Foreign.Storable as Storable

import GHC.Ptr

-- | A very simple and inefficient heap which allocates a
-- new pinned 'ByteArray' for each allocation. It never
-- releases any memory.
data Heap m root = Heap {
    heapBlockSize :: !Int
  , heapRoot      :: !(MutVar (PrimState m) (Ptr root))
  , heapBlocks    :: !(MutVar (PrimState m) [ByteArray])
  }

-- | Create a new heap and pass it to the given function.
-- Makes sure to keep the 'Heap' alive.
withHeap :: PrimMonad m => (Heap m root -> m a) -> m a
withHeap f = do
  heap <- newHeap 1
  r <- f heap
  touch heap
  return r

newHeap :: PrimMonad m => Int -> m (Heap m root)
newHeap blockSize = do

  rootRef  <- newMutVar nullPtr
  blockRef <- newMutVar []

  let heap =
        Heap { heapRoot = rootRef
             , heapBlockSize = blockSize
             , heapBlocks = blockRef
             }

  return heap

-- | Allocate a new 'Struct' on the given Heap. Basically creates
-- a new pinned 'ByteArray' for each allocation.
allocStruct
  :: forall m struct fields root .
     ( PrimMonad m
     , StructSize struct
     , struct ~ Struct fields
     )
  => Heap m root
  -> Proxy (Struct fields)
  -> m (Struct fields)
allocStruct heap _ = do
  mba <- newPinnedByteArray size
  ba <- unsafeFreezeByteArray mba
  modifyMutVar' (heapBlocks heap) (\s -> ba : s)
  let Addr mem = byteArrayContents ba
  return (Struct (Ptr mem))
  where
    size = structSize (Proxy :: Proxy struct)

allocArray
  :: forall m elem root .
     ( PrimMonad m
     , Layout elem
     )
  => Heap m root
  -> Proxy elem
  -> Int
  -> m (Array elem)
allocArray heap _ n = do
  mba <- newPinnedByteArray bytes
  ba <- unsafeFreezeByteArray mba
  modifyMutVar' (heapBlocks heap) (\s -> ba : s)
  let Addr mem = byteArrayContents ba
      op = Ptr mem
  unsafeIOToPrim (Storable.poke (castPtr op) (n :: Int))
  return (Array op)
  where
    bytes = size (Proxy :: Proxy Int) + n * size (Proxy :: Proxy elem)
