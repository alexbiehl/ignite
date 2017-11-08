{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ignite.BlockHeap (
    Heap
  , withHeap
  , allocStruct
  , allocArray
  ) where

import Ignite.Layout
import Ignite.Prim.Array
import Ignite.Prim.Struct

import Control.Monad.Primitive
import Data.IORef
import Data.Primitive.ByteArray
import Data.Primitive.MutVar
import Data.Primitive.Types
import Data.Proxy
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Foreign.Storable as Storable
import Data.Word

type BlockLayout =
  Struct '[ "nextBlock" := Ptr Word8 ]

nextBlockSel :: Selector "nextBlock" (Ptr Word8) BlockLayout
nextBlockSel = Selector

type Allocator =
  Struct '[ "nextFree"   := Ptr Word8
          , "endFree"    := Ptr Word8
          , "blockSize"  := Int
          , "firstBlock" := BlockLayout
          ]

nextFreeSel :: Selector "nextFree" (Ptr Word8) Allocator
nextFreeSel = Selector

endFreeSel :: Selector "endFree" (Ptr Word8) Allocator
endFreeSel = Selector

blockSizeSel :: Selector "blockSize" Int Allocator
blockSizeSel = Selector

firstBlockSel :: Selector "firstBlock" BlockLayout Allocator
firstBlockSel = Selector

allocBlock :: PrimMonad m => Int -> m (Ptr Word8)
allocBlock size = do
  unsafeIOToPrim (mallocBytes (size + blockLayoutSize))
  where
    blockLayoutSize = structSize (Proxy :: Proxy BlockLayout)

initAllocator
  :: forall a m . PrimMonad m
  => Int
  -> Ptr a
  -> m Allocator
initAllocator blockSize op = do
  writeField nextBlockSel blockLayout nullPtr
  writeField nextFreeSel  allocator firstFreeByte
  writeField endFreeSel   allocator (castPtr op `plusPtr` blockSize)
  writeField blockSizeSel allocator blockSize
  writeField firstBlockSel  allocator blockLayout
  return allocator
  where
    blockLayoutSize = structSize (Proxy :: Proxy BlockLayout)
    blockLayout = Struct (castPtr op)

    allocatorSize = structSize (Proxy :: Proxy Allocator)
    allocator = Struct (castPtr op `plusPtr` blockLayoutSize) :: Allocator

    firstFreeByte = op `plusPtr` (allocatorSize + blockLayoutSize)

withHeap :: PrimMonad m => Int -> (Heap m root -> m a) -> m a
withHeap blockSize f = do
  firstBlock <- allocBlock blockSize
  allocator  <- initAllocator blockSize firstBlock
  f (Heap allocator)

newtype Heap (m :: * -> *) root = Heap { getHeap :: Allocator }

allocStruct
  :: forall m struct fields root .
     ( PrimMonad m
     , StructSize struct
     , struct ~ Struct fields
     )
  => Heap m root
  -> Proxy (Struct fields)
  -> m (Struct fields)
allocStruct (Heap allocator) _ = do
  nextFree <- readField nextFreeSel allocator
  endFree  <- readField endFreeSel  allocator

  op <- if nextFree `plusPtr` size >= endFree
        then do blockSize <- readField blockSizeSel allocator
                newBlock  <- allocBlock (max size blockSize)
                let newBlockLayout = Struct (castPtr newBlock) :: BlockLayout
                Struct firstBlock <- readField firstBlockSel allocator
                writeField nextBlockSel newBlockLayout (castPtr firstBlock)
                writeField firstBlockSel allocator newBlockLayout
                let freeSpace = newBlock `plusPtr` blockLayoutSize
                writeField nextFreeSel allocator (freeSpace `plusPtr` size)
                writeField endFreeSel  allocator (newBlock `plusPtr` blockSize)
                return freeSpace
        else do writeField nextFreeSel allocator (nextFree `plusPtr` size)
                return nextFree

  return (Struct (castPtr op))
  where
    size = structSize (Proxy :: Proxy struct)

    blockLayoutSize = structSize (Proxy :: Proxy BlockLayout)

allocArray
  :: forall m elem root .
     ( PrimMonad m
     , Layout elem
     )
  => Heap m root
  -> Proxy elem
  -> Int
  -> m (Array elem)
allocArray (Heap allocator) _ n = do
  nextFree <- readField nextFreeSel allocator
  endFree  <- readField endFreeSel  allocator

  op <- if nextFree `plusPtr` bytes >= endFree
        then do blockSize <- readField blockSizeSel allocator
                newBlock  <- allocBlock (max bytes blockSize)
                let newBlockLayout = Struct (castPtr newBlock) :: BlockLayout
                Struct firstBlock <- readField firstBlockSel allocator
                writeField nextBlockSel newBlockLayout (castPtr firstBlock)
                writeField firstBlockSel allocator newBlockLayout
                let freeSpace = newBlock `plusPtr` blockLayoutSize
                writeField nextFreeSel allocator (freeSpace `plusPtr` bytes)
                writeField endFreeSel  allocator (newBlock `plusPtr` blockSize)
                return freeSpace
        else do writeField nextFreeSel allocator (nextFree `plusPtr` bytes)
                return nextFree

  unsafeIOToPrim (Storable.poke (castPtr op) (n :: Int))
  return (Array (castPtr op))
  where
    bytes = size (Proxy :: Proxy Int) + n * size (Proxy :: Proxy elem)
    blockLayoutSize = structSize (Proxy :: Proxy BlockLayout)
