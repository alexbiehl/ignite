{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
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

instance Layout Int where
  type Rep Int = Int

-- | A marker for fields in structure. Indexed by a
-- type level string.
data (label :: Symbol) := (value :: Type) = Field

instance Layout value => Layout (label := value) where
  type Rep (label := value) = Rep value

-- | A struct consists of a sequence of fields.
newtype Struct (fields :: [Type]) = Struct { getStruct :: Ptr (Struct fields) }
  deriving (Storable)

type family FieldType (field :: Symbol) (fields :: Type) :: Type where
  FieldType field (Struct (field := value ': fields)) = value
  FieldType field (Struct (label := value ': fields)) = FieldType field (Struct fields)

class FieldOffset (field :: Symbol) struct where
  fieldOffset :: Proxy field -> Proxy struct -> Int

instance {-# OVERLAPPING #-} ( Layout value )
        => FieldOffset field (Struct (field := value ': fields)) where
  fieldOffset _ _ = 0

instance ( Layout value
         , FieldOffset field (Struct fields))
        => FieldOffset field (Struct (label := value ': fields)) where
  fieldOffset wanted _ =
    sizeOf (undefined :: Rep value) + fieldOffset wanted (Proxy :: Proxy (Struct fields))

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

data Selector (label :: Symbol) value struct = Selector

instance Layout value => Layout (Selector label value struct) where
  type Rep (Selector label value struct) = Rep value

selectorOffset
  :: forall label value struct . FieldOffset label struct
  =>  Selector label value struct
  -> Int
selectorOffset _ = fieldOffset (Proxy :: Proxy label) (Proxy :: Proxy struct)

select
  :: ( FieldOffset label struct
     , Layout value
     )
  => Selector label value struct
  -> Struct fields
  -> IO (Rep value)
select sel (Struct op) = peekByteOff op offset
  where offset = selectorOffset sel

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

arrayListElems :: Layout elem => ArrayList elem -> IO (Array elem)
arrayListElems =
  select (Selector :: Selector "elems" (Array elem) (ArrayList elem))

allocArray
  :: Layout elem
  => Proxy elem
  -> Int
  -> IO (Array elem)
allocArray _ n = undefined

allocStruct
  :: (StructSize struct)
  => Proxy struct
  -> IO struct
allocStruct proxy = do
  print size
  return undefined
  where
    size :: Int
    size = structSize proxy
