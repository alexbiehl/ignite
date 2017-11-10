{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Ignite.Layout
import Ignite.Prim.Array
import Ignite.Prim.Struct
import Ignite.BlockHeap
import Ignite.BlockHeap.Prelude.ArrayList
import Ignite.BlockHeap.Prelude.HashMap

import Control.Monad.Primitive
import Data.Proxy
import Data.Foldable


test_monotonic :: IO ()
test_monotonic = withHeap 2048 $ \heap -> do

  hm <- newHashMap heap 16 0.75 :: IO (HashMap Int Int)
  alist <- newArrayList heap 20 :: IO (ArrayList Int)

  for_ [1..25] $ \i -> do
    insert heap hm i i
    arrayListAppend heap alist i

  for_ [0..24] $ \i -> do
    y <- Ignite.BlockHeap.Prelude.HashMap.lookup hm i
    print y
    x <- arrayListIndex alist i
    print x

test :: ArrayList elem -> IO (Array elem)
test alist = get alist #elems

test1 :: ArrayList Int -> IO (Array Int)
test1 = test

test2 :: Layout elem => ArrayList elem -> elem -> IO ()
test2 alist elem = do
  elems <- get alist #elems
  arrayUnsafeWrite elems 0 elem

test3 :: ArrayList Int -> Int -> IO ()
test3 = test2

main :: IO ()
main = test_monotonic
