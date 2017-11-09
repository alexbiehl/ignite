{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
module Ignite.Prelude.HashMap where

import Ignite.Heap
import Ignite.Layout
import Ignite.Prim.Array
import Ignite.Prim.Struct

import Ignite.Prelude.Hashing

import Control.Monad.Primitive
import Data.Bits
import Data.Proxy
import Foreign.Ptr
import Data.Word

type LoadFactor = Float

type Entry k v =
  Struct '[ "hash"  := Hash
          , "key"   := k
          , "value" := v
          , "next"  := Ptr Word8 -- hack
          ]

type HashMap k v =
  Struct '[ "table"      := Array (Entry k v)
          , "size"       := Int
          , "threshold"  := Int
          , "loadFactor" := Float
          , "modCount"   := Int
          ]

defaultInitialCapacity :: Int
defaultInitialCapacity = 16

newHashMap
  :: forall m root k v . (PrimMonad m, Layout k, Layout v)
  => Heap m root
  -> Int
  -> LoadFactor
  -> m (HashMap k v)
newHashMap heap _initialCapacity loadFactor = do
  this    <- allocStruct heap (Proxy :: Proxy (HashMap k v))
  entries <- allocArray heap (Proxy :: Proxy (Entry k v)) capacity

  set this #table entries
  set this #size 0
  set this #threshold (round (fromIntegral capacity * loadFactor))
  set this #loadFactor loadFactor
  set this #modCount 0

  return this
  where
    capacity = defaultInitialCapacity

class Equals a where
  isEqual :: PrimMonad m => a -> a -> m Bool

put
  :: forall m root k v . (PrimMonad m, Layout k, Layout v, Equals k, HashCode k)
  => Heap m root
  -> HashMap k v
  -> k
  -> v
  -> m ()
put heap this k v = do
  hash     <- hashWithSeed 0 k
  entries  <- get this #table
  capacity <- arrayLength entries
  let
    i = indexOf hash capacity

    loop e
      | isNullStruct e = addEntry heap this hash k v i
      | otherwise = do
          entryHash <- get e #hash
          entryKey  <- get e #key

          if entryHash == hash
            then do eq <- isEqual entryKey k
                    if eq
                      then do oldVal <- get e #value
                              set e #value v
                      else addEntry heap this hash k v i
            else do next <- get e #next
                    loop (unsafeFromPtr next)

  entry <- arrayUnsafeIndex entries i
  loop entry
  return ()

addEntry
  :: forall m root k v . (PrimMonad m, Layout k, Layout v, Equals k, HashCode k)
  => Heap m root
  -> HashMap k v
  -> Hash
  -> k
  -> v
  -> Int
  -> m ()
addEntry heap this hash key value bucketIndex = do
  entries  <- get this #table
  Struct entry <- arrayUnsafeIndex entries bucketIndex
  newEntry <- allocStruct heap (Proxy :: Proxy (Entry k v))
  set newEntry #hash hash
  set newEntry #key key
  set newEntry #value value
  set newEntry #next (castPtr entry)
  arrayUnsafeWrite entries bucketIndex newEntry

  size <- get this #size
  let size' = size + 1
  set this #size size'

  threshold <- get this #threshold
  if (size' >= threshold)
    then do cap <- arrayLength entries
            resize heap this (2 * cap)
    else return ()

resize
  :: forall m root k v . (PrimMonad m, Layout k, Layout v, HashCode k, Equals k)
  => Heap m root
  -> HashMap k v
  -> Int
  -> m ()
resize heap this newCapacity = do
  oldTable <- get this #table
  oldCapacity <- arrayLength oldTable
  newTable <- allocArray heap (Proxy :: Proxy (Entry k v)) newCapacity

  let
    transfer i
      | i < oldCapacity = do
          entry <- arrayUnsafeIndex oldTable i
          if not (isNullStruct entry)
            then do arrayUnsafeWrite oldTable i nullStruct
                    let
                      transfer1 e
                        | isNullStruct e = return ()
                        | otherwise = do
                            hash <- get e #hash
                            next <- get e #next
                            let j = indexOf hash newCapacity
                            Struct next' <- arrayUnsafeIndex newTable j
                            set e #next (castPtr next')
                            arrayUnsafeWrite newTable j e
                            transfer1 (unsafeFromPtr next)

                    transfer1 entry
            else return ()
          transfer (i + 1)
      | otherwise = return ()

  transfer 0

lookup
  :: forall m root k v . (PrimMonad m, Layout k, Layout v, HashCode k, Equals k)
  => Heap m root
  -> HashMap k v
  -> k
  -> m (Maybe v)
lookup heap this k = do
  hash     <- hashWithSeed 0 k
  entries  <- get this #table
  capacity <- arrayLength entries
  entry    <- arrayUnsafeIndex entries (indexOf hash capacity)

  let
    loop :: Entry k v -> m (Maybe v)
    loop e
      | isNullStruct e = return Nothing
      | otherwise      = do
          entryHash <- get e #hash
          entryKey  <- get e #key

          if entryHash == hash
            then do key <- get e #key
                    val <- get e #value
                    eq  <- isEqual k key
                    if eq
                      then return (Just val)
                      else do next <- get e #next
                              loop (unsafeFromPtr next)
            else do next <- get e #next
                    loop (unsafeFromPtr next)
  loop entry

-- | Index for 'Hash' in the table
indexOf :: Hash -> Int -> Int
indexOf hash length = hash .&. (length - 1)
