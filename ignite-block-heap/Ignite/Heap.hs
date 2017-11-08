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
module Ignite.Heap (
    Heap
  , withHeap
  , allocStruct
  , allocArray
  ) where

import Ignite.Layout
import Ignite.Prim.Array
import Ignite.Prim.Struct

import Control.Exception
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

-- | Memory is allocated from 'Block's. 'Block's are chained through
-- the 'nextBlock' field. This is a newtype since GHC doesn't like
-- infinite type synonyms.
newtype Block = Block { getBlock :: Struct '[ "nextBlock" := Block ] }

instance Layout Block where
  type Rep Block = Ptr (Struct '[ "nextBlock" := Block ])
  size _ = size (Proxy :: Proxy (Rep Block))
  peek op off = fmap Block (peek (castPtr op) off)
  poke op off a = poke (castPtr op) off (getBlock a)

nullBlock :: Block
nullBlock = Block (unsafeFromPtr nullPtr)

blockHeaderSize :: Int
blockHeaderSize =
  -- FIXME: find a way to get to the type of a Block
  structSize (Proxy :: Proxy (Struct '[ "nextBlock" := Block ]))

-- | We want to allocate new memory through malloc or the like
-- so we need a way to cast a raw pointer to something which looks
-- like a block. Hopefully op is big enough to hold the nextBlock
-- pointer!
unsafeBlockFromPointer
  :: PrimMonad m
  => Ptr a
  -> Block
  -> m Block
unsafeBlockFromPointer op nextBlock = do
  set block #nextBlock nextBlock
  return (Block block)
  where
    block = unsafeFromPtr op

blockRawMemory :: Block -> Ptr Word8
blockRawMemory (Block (Struct block)) =
  block `plusPtr` blockHeaderSize

type Allocator =
  Struct '[ "hp"        := Ptr Word8
          , "hpLim"     := Ptr Word8
          , "blockSize" := Int
          , "blocks"    := Block
          ]

data OutOfMemoryException = OutOfMemoryException
  deriving (Eq, Show)

instance Exception OutOfMemoryException

allocatorSize :: Int
allocatorSize = structSize (Proxy :: Proxy Allocator)

allocRawMemory :: PrimMonad m => Int -> m (Ptr Word8)
allocRawMemory size = unsafeIOToPrim $ do
  op <- mallocBytes size
  if op == nullPtr
    then throwIO OutOfMemoryException
    else return op

-- | Take a bootstrap 'Block' and put the allocator in the first bytes the
-- blocks memory.
bootstrapAllocator
  :: PrimMonad m
  => Int
  -> Block
  -> m Allocator
bootstrapAllocator blockSize block = do
  set allocator #hp    (blockRawMemory block `plusPtr` allocatorSize)
  set allocator #hpLim (blockRawMemory block `plusPtr` (blockSize - blockHeaderSize))
  set allocator #blockSize blockSize
  set allocator #blocks block
  return allocator
  where
    allocator = unsafeFromPtr (blockRawMemory block)

-- | The current block is full, allocate a new one and chain the old
-- ones to it. We explicitly pass HpAlloc here as we want to be able
-- to allocate more than blocksize if necessary.
allocateNewBlock :: PrimMonad m => Allocator -> Int -> m (Ptr Word8)
allocateNewBlock allocator hpAlloc = do
  blockMem  <- allocRawMemory hpAlloc
  blocks    <- get allocator #blocks
  newBlock  <- unsafeBlockFromPointer blockMem blocks

  let hp = blockRawMemory newBlock
  set allocator #hp     hp
  set allocator #hpLim  (hp `plusPtr` (hpAlloc - blockHeaderSize))
  set allocator #blocks newBlock
  return hp

newtype Heap (m :: * -> *) root = Heap { getHeap :: Allocator }

alloc :: PrimMonad m => Allocator -> Int -> m (Ptr Word8)
alloc allocator hpAlloc = do
  hp    <- get allocator #hp
  hpLim <- get allocator #hpLim
  if hp `plusPtr` hpAlloc >= hpLim
    then do blockSize <- get allocator #blockSize
            allocateNewBlock
              allocator (if hpAlloc > blockSize - blockHeaderSize
                          then hpAlloc + blockHeaderSize else blockSize)
    else do set allocator #hp (hp `plusPtr` hpAlloc)
            return (hp `plusPtr` hpAlloc)

allocStruct
  :: forall m struct fields root .
     ( PrimMonad m
     , struct ~ Struct fields
     , StructSize struct
     )
  => Heap m root
  -> Proxy (Struct fields)
  -> m (Struct fields)
allocStruct (Heap allocator) struct = do
  hp <- alloc allocator hpAlloc
  return (unsafeFromPtr hp)
  where
    hpAlloc :: Int
    hpAlloc = structSize struct

allocArray
  :: forall m elem root .
     ( PrimMonad m
     , Layout elem
     )
  => Heap m root
  -> Proxy elem
  -> Int
  -> m (Array elem)
allocArray (Heap allocator) elem n = do
  hp <- alloc allocator hpAlloc
  arrayUnsafeFromPtr hp n
  where
    hpAlloc = arraySize elem n

withHeap :: PrimMonad m => Int -> (Heap m root -> m a) -> m a
withHeap blockSize f = do
  -- the first block needs some space for the allocator
  op    <- allocRawMemory (max blockSize (blockHeaderSize + allocatorSize))
  block <- unsafeBlockFromPointer op nullBlock
  allocator <- bootstrapAllocator (max blockSize blockHeaderSize) block
  r <- f (Heap allocator)
  -- FIXME: deallocate the heap
  -- FIXME: exception safety
  return r
