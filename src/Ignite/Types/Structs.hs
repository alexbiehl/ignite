{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Ignite.Types.Structs where

import GHC.TypeLits

import Data.Kind
import Data.Proxy

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

data Heap root = Heap

-- | Every object in Ignite needs a heap layout
-- it defines the representation as well as the
-- static size of the heap object
class Storable (Rep a) => Layout a where
  type Rep a

-- | A marker for fields in structure. Indexed by a
-- type level string.
data (label :: Symbol) := (value :: Type) = Field

instance Layout value => Layout (label := value) where
  type Rep (label := value) = Rep value

-- | A struct consists of a sequence of fields.
newtype Struct (fields :: [Type]) = Struct { getStruct :: Ptr (Struct fields) }
  deriving (Storable)

-- | A helper class for determining the size of a 'Struct'
class StructSize a where
  structSize :: Proxy a -> Int

instance StructSize (Struct '[]) where
  structSize _ = 0

instance (Layout field, StructSize (Struct fields))
    => StructSize (Struct (field ': fields)) where
  structSize _ =
    sizeOf (undefined :: Rep field)
      + structSize (Proxy :: Proxy (Struct fields))

-- | A sequence of elements which knows it length.
newtype Array elem = Array { getArray :: Ptr (Rep elem) }
  deriving (Storable)

instance Layout elem => Layout (Array elem) where
  type Rep (Array elem) = Array elem

arrayLength :: Array elem -> IO Int
arrayLength (Array op) = peek (castPtr op)

unsafeArrayElemAt :: Layout elem => Array elem -> Int -> IO (Rep elem)
unsafeArrayElemAt (Array op) index = peekElemOff arrayElems index
  where
    -- skip the length field of the array
    arrayElems :: Ptr elem
    arrayElems = op `plusPtr` (sizeOf (undefined :: Int))

type ArrayList elem = Struct '[ "size" := Int, "elems" := Array elem ]

arrayList :: Proxy (ArrayList elem)
arrayList = Proxy

allocStruct
  :: (StructSize struct, Layout struct)
  => Proxy struct
  -> IO struct
allocStruct proxy = do
  print size
  return undefined
  where
    size :: Int
    size = structSize proxy
