{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ignite.Array where

import Ignite.Layout

import Control.Monad.Primitive
import Data.Proxy
import Foreign.Ptr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (peekElemOff, pokeElemOff)

-- | A sequence of elements.
--    * 'Array's know their lengths
--    * 'Array's support indexing in constant time
newtype Array elem = Array { getArray :: Ptr (Array elem) }

instance Layout (Array elem) where

  -- | Keep representation for 'Array' is opaque
  type Rep (Array elem) = Array elem

  size _ = size (Proxy :: Proxy (Ptr (Array elem)))
  {-# INLINE size #-}

  peek op off = fmap Array (peek (castPtr op) off)
  {-# INLINE peek #-}

  poke op off a = poke (castPtr op) off (getArray a)
  {-# INLINE poke #-}

-- | Returns the length of the 'Array'. This is a constant time operation.
arrayLength :: PrimMonad m => Array elem -> m Int
arrayLength (Array op) = peek (castPtr op) 0
{-# INLINE arrayLength #-}

arrayUnsafeWithElems
  :: forall m a elem . (PrimMonad m, Layout elem)
  => Array elem
  -> (Int -> Ptr (Rep elem) -> m a)
  -> m a
arrayUnsafeWithElems a@(Array op) f = do
  len <- arrayLength a
  f len (op `plusPtr` size (Proxy :: Proxy Int))
{-# INLINE arrayUnsafeWithElems #-}

-- | Indexes into the array. No bound checks are performed. This is a
-- constant time operation.
arrayUnsafeIndex
  :: forall m elem . (PrimMonad m, Layout elem)
  => Array elem
  -> Int
  -> m elem
arrayUnsafeIndex (Array op) index =
  peek (castPtr op) (lenSize + elemSize * index)
  where
    elemSize = size (Proxy :: Proxy elem)
    lenSize  = size (Proxy :: Proxy Int)
{-# INLINE arrayUnsafeIndex #-}

arrayUnsafeWrite
  :: forall m elem . (PrimMonad m, Layout elem)
  => Array elem
  -> Int
  -> elem
  -> m ()
arrayUnsafeWrite (Array op) index elem =
  poke (castPtr op) (lenSize + elemSize * index) elem
  where
    elemSize = size (Proxy :: Proxy elem)
    lenSize  = size (Proxy :: Proxy Int)
{-# INLINE arrayUnsafeWrite #-}

unsafeArrayCopy
  :: forall m elem . (PrimMonad m, Layout elem)
  => Array elem
  -> Int
  -> Array elem
  -> Int
  -> Int
  -> m ()
unsafeArrayCopy (Array src) srcOff (Array dest) destOff len =
  unsafeIOToPrim (copyBytes
                   (dest `plusPtr` (lenSize + destOff * elemSize))
                   (src  `plusPtr` (lenSize + srcOff * elemSize))
                   (len * elemSize))
  where
    elemSize = size (Proxy :: Proxy elem)
    lenSize  = size (Proxy :: Proxy Int)
