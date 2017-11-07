{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ignite.Array where

import Ignite.Layout

import Control.Monad.Primitive
import Data.Proxy
import Foreign.Ptr
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

-- | Indexes into the array. No bound checks are performed. This is a
-- constant time operation.
arrayUnsafeIndex
  :: forall m elem . (PrimMonad m, Layout elem)
  => Array elem
  -> Int
  -> m (Rep elem)
arrayUnsafeIndex (Array op) index =
  peek (castPtr op) (lenSize + elemSize * index)
  where
    elemSize = size (Proxy :: Proxy (Rep elem))
    lenSize  = size (Proxy :: Proxy (Rep Int))
{-# INLINE arrayUnsafeIndex #-}
