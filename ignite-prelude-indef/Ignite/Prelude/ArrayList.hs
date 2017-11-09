{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
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

newArrayList
  :: forall m elem root . (PrimMonad m, Layout elem)
  => Heap m root
  -> Int
  -> m (ArrayList elem)
newArrayList heap capacity = do
  arrayList <- allocStruct heap (Proxy :: Proxy (ArrayList elem))
  array     <- allocArray heap (Proxy :: Proxy elem) capacity
  set arrayList #size  0
  set arrayList #elems array
  return arrayList

arrayListSize
  :: forall m elem root . (PrimMonad m)
  => ArrayList elem
  -> m Int
arrayListSize alist = get alist #size

arrayListCapacity
  :: forall m elem root . (PrimMonad m)
  => ArrayList elem
  -> m Int
arrayListCapacity alist = do
  arr <- get alist #elems
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
  arr  <- get alist #elems

  if size < cap
    then do arrayUnsafeWrite arr size elem
            set alist #size (size + 1)
    else arrayListResize heap alist >> arrayListAppend heap alist elem

arrayListResize
  :: forall m root elem . (PrimMonad m, Layout elem)
  => Heap m root
  -> ArrayList elem
  -> m ()
arrayListResize heap alist = do
  size <- arrayListSize alist
  newArr <- allocArray heap (Proxy :: Proxy elem) (2 * size)
  oldArr <- get alist #elems
  unsafeArrayCopy oldArr 0 newArr 0 size
  set alist #elems newArr

arrayListIndex
  :: forall m elem . (PrimMonad m, Layout elem)
  => ArrayList elem
  -> Int
  -> m elem
arrayListIndex alist i = do
  arr <- get alist #elems
  arrayUnsafeIndex arr i
