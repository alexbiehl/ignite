{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module Ignite.Example where

import Ignite.Layout
import Ignite.Prim.Array
import Ignite.Prim.Struct
import Ignite.BlockHeap

import Control.Monad.Primitive
import Data.Proxy
import Data.Foldable

type ArrayList elem = Struct '[ "size" := Int, "elems" := Array elem ]

newArrayList
  :: forall m elem root . (PrimMonad m, Layout elem)
  => Heap m root
  -> Int
  -> m (ArrayList elem)
newArrayList heap capacity = do
  arrayList <- allocStruct heap (Proxy :: Proxy (ArrayList elem))
  array     <- allocArray heap (Proxy :: Proxy elem) capacity
  writeField arrayList #size 0
  writeField arrayList #elems array
  return arrayList

arrayListSize
  :: forall m elem root . (PrimMonad m)
  => ArrayList elem
  -> m Int
arrayListSize alist = readField alist #size

arrayListCapacity
  :: forall m elem root . (PrimMonad m)
  => ArrayList elem
  -> m Int
arrayListCapacity alist = do
  arr <- readField alist #elems
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
  arr  <- readField alist #elems

  if size < cap
    then do arrayUnsafeWrite arr size elem
            writeField alist #size (size + 1)
    else arrayListResize heap alist >> arrayListAppend heap alist elem

arrayListResize
  :: forall m root elem . (PrimMonad m, Layout elem)
  => Heap m root
  -> ArrayList elem
  -> m ()
arrayListResize heap alist = do
  size <- arrayListSize alist
  newArr <- allocArray heap (Proxy :: Proxy elem) (2 * size)
  oldArr <- readField alist #elems
  unsafeArrayCopy oldArr 0 newArr 0 size
  writeField alist #elems newArr

arrayListIndex
  :: forall m elem . (PrimMonad m, Layout elem)
  => ArrayList elem
  -> Int
  -> m elem
arrayListIndex alist i = do
  arr  <- readField alist #elems
  arrayUnsafeIndex arr i

test_monotonic :: IO ()
test_monotonic = withHeap 1024 $ \heap -> do
  alist <- newArrayList heap 20 :: IO (ArrayList Int)

  for_ [1..25] $ \i ->
    arrayListAppend heap alist i

  for_ [0..24] $ \i -> do
    x <- arrayListIndex alist i
    print x

test :: ArrayList elem -> IO (Array elem)
test alist = readField alist #elems

test1 :: ArrayList Int -> IO (Array Int)
test1 = test

test2 :: Layout elem => ArrayList elem -> elem -> IO ()
test2 alist elem = do
  elems <- readField alist #elems
  arrayUnsafeWrite elems 0 elem

test3 :: ArrayList Int -> Int -> IO ()
test3 = test2
