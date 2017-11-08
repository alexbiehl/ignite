{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
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

nextBlockSel :: Selector "nextBlock"
nextBlockSel = Selector

type Allocator =
  Struct '[ "nextFree"   := Ptr Word8
          , "endFree"    := Ptr Word8
          , "blockSize"  := Int
          , "firstBlock" := BlockLayout
          ]

nextFreeSel :: Selector "nextFree"
nextFreeSel = Selector

endFreeSel :: Selector "endFree"
endFreeSel = Selector

blockSizeSel :: Selector "blockSize"
blockSizeSel = Selector

firstBlockSel :: Selector "firstBlock"
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
  writeField blockLayout #nextBlock nullPtr

  writeField allocator #nextFree   firstFreeByte
  writeField allocator #endFree    (castPtr op `plusPtr` blockSize)
  writeField allocator #blockSize  blockSize
  writeField allocator #firstBlock blockLayout
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
  nextFree <- readField allocator #nextFree
  endFree  <- readField allocator #endFree

  op <- if nextFree `plusPtr` size >= endFree
        then do blockSize <- readField allocator #blockSize
                newBlock  <- allocBlock (max size blockSize)
                let newBlockLayout = Struct (castPtr newBlock) :: BlockLayout
                Struct firstBlock <- readField allocator #firstBlock :: m BlockLayout
                writeField newBlockLayout #nextBlock (castPtr firstBlock)
                writeField allocator #firstBlock newBlockLayout
                let freeSpace = newBlock `plusPtr` blockLayoutSize
                writeField allocator #nextFree (freeSpace `plusPtr` size)
                writeField allocator #endFree (newBlock `plusPtr` blockSize)
                return freeSpace
        else do writeField allocator #nextFree (nextFree `plusPtr` size)
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
  nextFree <- readField allocator #nextFree
  endFree  <- readField allocator #endFree

  op <- if nextFree `plusPtr` bytes >= endFree
        then do blockSize <- readField allocator #blockSize
                newBlock  <- allocBlock (max bytes blockSize)
                let newBlockLayout = Struct (castPtr newBlock) :: BlockLayout
                Struct firstBlock <- readField allocator #firstBlock
                writeField newBlockLayout #nextBlock (castPtr firstBlock)
                writeField allocator #firstBlock newBlockLayout
                let freeSpace = newBlock `plusPtr` blockLayoutSize
                writeField allocator #nextFree (freeSpace `plusPtr` bytes)
                writeField allocator #endFree (newBlock `plusPtr` blockSize)
                return freeSpace
        else do writeField allocator #nextFree (nextFree `plusPtr` bytes)
                return nextFree

  unsafeIOToPrim (Storable.poke (castPtr op) (n :: Int))
  return (Array (castPtr op))
  where
    bytes = size (Proxy :: Proxy Int) + n * size (Proxy :: Proxy elem)
    blockLayoutSize = structSize (Proxy :: Proxy BlockLayout)
