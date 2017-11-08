{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ignite.Layout where

import Control.Monad.Primitive
import Data.Proxy
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Ptr
import qualified Foreign.Storable as Storable

-- | A layout defines the in-memory representation for a value.
class Layout a where

  -- | Type of the in-memory representation
  type family Rep a

  -- | Determines the size of the representation
  size
   :: Proxy a
   -> Int

  default size
    :: Storable.Storable (Rep a)
    => Proxy a
    -> Int
  size _ = Storable.sizeOf (undefined :: Rep a)
  {-# INLINE size #-}

  -- | Reads a value from a pointer and an offset
  peek
    :: PrimMonad m
    => Ptr a
    -> Int
    -> m a

  default peek
    :: (PrimMonad m, a ~ Rep a, Storable.Storable (Rep a))
    => Ptr a
    -> Int
    -> m a
  peek op off = unsafeIOToPrim (Storable.peekByteOff op off)
  {-# INLINE peek #-}

  -- | Writes a values to pointer at offset.
  poke
    :: PrimMonad m
    => Ptr a
    -> Int
    -> a
    -> m ()

  default poke
    :: (PrimMonad m, a ~ Rep a, Storable.Storable (Rep a))
    => Ptr a
    -> Int
    -> a
    -> m ()
  poke op off a = unsafeIOToPrim (Storable.pokeByteOff op off a)
  {-# INLINE poke #-}

instance Layout (Ptr a) where
  type Rep (Ptr a) = Ptr a

instance Layout Int where
  type Rep Int = Int

instance Layout Word where
  type Rep Word = Word

instance Layout Float where
  type Rep Float = Float

instance Layout Bool where
  type Rep Bool = Bool

instance Layout Word8 where
  type Rep Word8 = Word8

instance Layout Word16 where
  type Rep Word16 = Word16

instance Layout Word32 where
  type Rep Word32 = Word32

instance Layout Word64 where
  type Rep Word64 = Word64
