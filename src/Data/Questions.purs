module Data.Questions 
    ( Questions
    , empty
    , index, (!!)
    , size
    , shuffleQuestions
    ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Array as A
import Data.Maybe (Maybe)
import Data.Question (Question, shuffleAnswers)
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect, liftEffect)
import Utils.Shuffle (shuffle)


newtype Questions =
    Questions (Array Question)


instance decodeJsonQuestions :: DecodeJson Questions where
    decodeJson json = do
        obj <- decodeJson json
        questions <- obj .: "results"
        pure $ Questions questions


empty :: Questions
empty = 
    Questions []


infixl 8 index as !!

index :: Questions -> Int -> Maybe Question
index (Questions qs) n = 
    qs A.!! n


size :: Questions -> Int
size (Questions qs) =
    A.length qs


shuffleQuestions :: forall m. MonadEffect m => Questions -> m Questions
shuffleQuestions (Questions questions) = do
    shuffledAnswers <- traverse shuffleAnswers questions
    Questions <$> (liftEffect $ shuffle shuffledAnswers)