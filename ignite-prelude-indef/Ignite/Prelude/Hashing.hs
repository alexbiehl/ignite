module Ignite.Prelude.Hashing where

import Control.Monad.Primitive

type Hash = Int

type Seed = Int

class HashCode a where
  hashWithSeed :: PrimMonad m => Seed -> a -> m Hash
