module Ignite.Prelude.Hashing where

import Control.Monad.Primitive
import Data.Hashable

type Hash = Int

type Seed = Int

class HashCode a where
  hashWithSeed :: PrimMonad m => Seed -> a -> m Hash

instance HashCode Int where
  hashWithSeed = hashableHashcode

hashableHashcode :: (Hashable a, PrimMonad m) => Seed -> a -> m Hash
hashableHashcode seed a = return (hashWithSalt seed a)
