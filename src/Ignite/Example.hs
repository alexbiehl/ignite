{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ignite.Example where

import Ignite.Layout
import Ignite.Array
import Ignite.Struct

type ArrayList elem = Struct '[ "size" := Int, "elems" := Array elem ]

arrayListElemsSelector :: Selector "elems" (Array elem) (ArrayList elem)
arrayListElemsSelector = Selector

test :: ArrayList elem -> IO (Array elem)
test = readField arrayListElemsSelector

test1 :: ArrayList Int -> IO (Array Int)
test1 = test

test2 :: Layout elem => ArrayList elem -> elem -> IO ()
test2 alist elem = do
  elems <- readField arrayListElemsSelector alist
  arrayUnsafeWrite elems 0 elem

test3 :: ArrayList Int -> Int -> IO ()
test3 = test2
