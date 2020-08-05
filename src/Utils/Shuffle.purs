module Utils.Shuffle (shuffle) where

import Prelude

import Data.Array (fromFoldable, sortBy, toUnfoldable, zip)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Tuple (fst, snd)
import Data.Unfoldable (class Unfoldable, replicateA)
import Effect (Effect)
import Effect.Random (randomInt)


shuffle :: forall f a. Foldable f => Unfoldable f => f a -> Effect (f a)
shuffle items = do
    let itemArray = fromFoldable items
    let size = Array.length itemArray
    randoms <- randomInts size 0 top
    let shuffled = snd <$> sortBy (comparing fst) (zip randoms itemArray)
    pure $ toUnfoldable shuffled


randomInts :: forall f. Unfoldable f => Traversable f => Int -> Int -> Int -> Effect (f Int)
randomInts n lower upper = do
    replicateA n (randomInt lower upper)