module Data.Questions 
    ( Questions
    , empty
    , index, (!!)
    , size
    , modifyAt
    , setAnswerAt
    , shuffleQuestions
    , getQuestionStates
    ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Array as A
import Data.Maybe (Maybe, fromMaybe)
import Data.Question (Answer, Question, QuestionState)
import Data.Question as Q
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


modifyAt :: Int -> (Question -> Question) -> Questions -> Maybe Questions
modifyAt ind modify (Questions qs) =
    Questions <$> A.modifyAt ind modify qs


setAnswerAt :: Int -> Answer -> Questions -> Questions
setAnswerAt ind answ questions = fromMaybe questions $
    modifyAt ind (Q.setAnswer answ) questions


shuffleQuestions :: forall m. MonadEffect m => Questions -> m Questions
shuffleQuestions (Questions questions) = do
    shuffledAnswers <- traverse Q.shuffleAnswers questions
    Questions <$> (liftEffect $ shuffle shuffledAnswers)


getQuestionStates :: Questions -> Array QuestionState
getQuestionStates (Questions qs) =
    map Q.getState qs