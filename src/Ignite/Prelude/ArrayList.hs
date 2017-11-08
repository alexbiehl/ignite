{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Ignite.Prelude.ArrayList (
    ArrayList
  , newArrayList
  , arrayListSize
  , arrayListAppend
  , arrayListIndex
  ) where

import Ignite.Heap
import Ignite.Layout
import Ignite.Prim.Array
import Ignite.Prim.Struct

import Control.Monad.Primitive
import Data.Proxy

type ArrayList elem = Struct '[ "size" := Int, "elems" := Array elem ]

arrayListElemsSelector :: Selector "elems" (Array elem) (ArrayList elem)
arrayListElemsSelector = Selector

arrayListSizeSelector :: Selector "size" Int (ArrayList elem)
arrayListSizeSelector = Selector

newArrayList
  :: forall m elem root . (PrimMonad m, Layout elem)
  => Heap m root
  -> Int
  -> m (ArrayList elem)
newArrayList heap capacity = do
  arrayList <- allocStruct heap (Proxy :: Proxy (ArrayList elem))
  array     <- allocArray heap (Proxy :: Proxy elem) capacity
  writeField arrayListSizeSelector arrayList 0
  writeField arrayListElemsSelector arrayList array
  return arrayList

arrayListSize
  :: forall m elem root . (PrimMonad m)
  => ArrayList elem
  -> m Int
arrayListSize alist = readField arrayListSizeSelector alist

arrayListCapacity
  :: forall m elem root . (PrimMonad m)
  => ArrayList elem
  -> m Int
arrayListCapacity alist = do
  arr <- readField arrayListElemsSelector alist
  arrayLength arr

arrayListAppend
  :: forall m elem root . (PrimMonad m, Layout elem)
  => Heap m root
  -> ArrayList elem
  -> elem
  -> m ()
arrayListAppend heap alist elem = do
  size <- arrayListSize alist
  cap  <- arrayListCapacity alist
  arr  <- readField arrayListElemsSelector alist

  if size < cap
    then do arrayUnsafeWrite arr size elem
            writeField arrayListSizeSelector alist (size + 1)
    else arrayListResize heap alist >> arrayListAppend heap alist elem

arrayListResize
  :: forall m root elem . (PrimMonad m, Layout elem)
  => Heap m root
  -> ArrayList elem
  -> m ()
arrayListResize heap alist = do
  size <- arrayListSize alist
  newArr <- allocArray heap (Proxy :: Proxy elem) (2 * size)
  oldArr <- readField arrayListElemsSelector alist
  unsafeArrayCopy oldArr 0 newArr 0 size
  writeField arrayListElemsSelector alist newArr

arrayListIndex
  :: forall m elem . (PrimMonad m, Layout elem)
  => ArrayList elem
  -> Int
  -> m elem
arrayListIndex alist i = do
  arr  <- readField arrayListElemsSelector alist
  arrayUnsafeIndex arr i