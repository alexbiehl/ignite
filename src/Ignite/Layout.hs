{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Ignite.Layout where

import Control.Monad.Primitive
import Data.Proxy
import Foreign.Ptr
import qualified Foreign.Storable as Storable


-- | A layout defines the in-memory representation for a value.
class Layout a where

  -- | Type of the in-memory representation
  type family Rep a = r | r -> a

  -- | Determines the size of the representation
  size
   :: Proxy (Rep a)
   -> Int

  default size
    :: Storable.Storable (Rep a)
    => Proxy (Rep a)
    -> Int
  size _ = Storable.sizeOf (undefined :: Rep a)
  {-# INLINE size #-}

  -- | Reads a value from a pointer and an offset
  peek
    :: PrimMonad m
    => Ptr (Rep a)
    -> Int
    -> m (Rep a)

  default peek
    :: (PrimMonad m, Storable.Storable (Rep a))
    => Ptr (Rep a)
    -> Int
    -> m (Rep a)
  peek op off = unsafeIOToPrim (Storable.peekByteOff op off)
  {-# INLINE peek #-}

  -- | Writes a values to pointer at offset.
  poke
    :: PrimMonad m
    => Ptr (Rep a)
    -> Int
    -> (Rep a)
    -> m ()

  default poke
    :: (PrimMonad m, Storable.Storable (Rep a))
    => Ptr (Rep a)
    -> Int
    -> (Rep a)
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
