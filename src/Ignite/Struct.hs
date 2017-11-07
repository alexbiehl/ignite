{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ignite.Struct where

import Ignite.Layout

import Control.Monad.Primitive
import Data.Kind
import Data.Proxy
import Foreign.Ptr

import GHC.TypeLits

newtype Struct (fields :: [Type]) = Struct { getStruct :: Ptr (Struct fields) }

instance Layout (Struct fields) where

  type Rep (Struct fields) = Struct fields

  size _ = size (Proxy :: Proxy (Ptr (Struct fields)))
  {-# INLINE size #-}

  peek op off = fmap Struct (peek (castPtr op) off)
  {-# INLINE peek #-}

  poke op off a = poke (castPtr op) off (getStruct a)
  {-# INLINE poke #-}

class StructSize a where
  structSize :: Proxy a -> Int

instance StructSize (Struct '[]) where
  structSize _ = 0

instance (Layout field, StructSize (Struct fields))
  => StructSize (Struct (field ': fields)) where
  structSize _ =
    size (Proxy :: Proxy field) +
      structSize (Proxy :: Proxy (Struct fields))

-- | Fields of structures are represented through '(:=)'.
data (label :: Symbol) := (rep :: Type) = Field

instance Layout rep => Layout (label := rep) where

  type Rep (label := rep) = Rep rep

  size _ = size (Proxy :: Proxy rep)

  peek op off = peek (castPtr op) off

  poke op off a = poke (castPtr op) off a

-- | Calculates the byte offset of a field in a structure.
class FieldOffset (field :: Symbol) struct where
  fieldOffset :: Proxy field -> Proxy struct -> Int

instance {-# OVERLAPPING #-} ( Layout value )
        => FieldOffset field (Struct (field := value ': fields)) where
  fieldOffset _ _ = 0
  {-# INLINE fieldOffset #-}

instance ( Layout value
         , FieldOffset field (Struct fields))
        => FieldOffset field (Struct (label := value ': fields)) where
  fieldOffset wanted _ =
    size (Proxy :: Proxy value) +
      fieldOffset wanted (Proxy :: Proxy (Struct fields))
  {-# INLINE fieldOffset #-}

-- | Calculates the type of a specific field
type family FieldType (field :: Symbol) (fields :: Type) :: Type where
  FieldType field (Struct (field := value ': fields)) = value
  FieldType field (Struct (label := value ': fields)) = FieldType field (Struct fields)

data Selector (field :: Symbol) fieldType struct = Selector

-- | Reads the value of a field.
readField
  :: forall field fields fieldType struct m .
     ( PrimMonad m
     , FieldOffset field struct
     , fieldType ~ FieldType field struct
     , Layout fieldType
     )
  => Selector field fieldType struct -- ^ field selector
  -> Struct fields                   -- ^ struct from which to read
  -> m fieldType                     -- ^ the read value
readField _ (Struct op) = peek (castPtr op) offset
  where
    offset = fieldOffset (Proxy :: Proxy field) (Proxy :: Proxy struct)

writeField
  :: forall field fields fieldType struct m .
     ( PrimMonad m
     , FieldOffset field struct
     , fieldType ~ FieldType field struct
     , Layout fieldType
     )
  => Selector field fieldType struct -- ^ field selector
  -> Struct fields                   -- ^ struct to which to write
  -> fieldType                       -- ^ the value to be written
  -> m ()
writeField _ (Struct op) a = poke (castPtr op) offset a
  where
    offset = fieldOffset (Proxy :: Proxy field) (Proxy :: Proxy struct)
