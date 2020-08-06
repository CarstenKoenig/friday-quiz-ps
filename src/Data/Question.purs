module Data.Question
( Question (..)
, Answer
, Type (..)
, Difficulty (..)
, shuffleAnswers
) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Array ((:))
import Data.Either (Either(..))
import Data.String as String
import Effect.Class (class MonadEffect, liftEffect)
import Utils.Shuffle (shuffle)


data Type
    = YesNo
    | MultipleChoice


instance decodeJsonType :: DecodeJson Type where
    decodeJson json = do
        str <- decodeJson json
        case String.toLower str of
            "boolean" -> pure YesNo
            "multiple" -> pure MultipleChoice
            _ -> Left $ UnexpectedValue json


data Difficulty
    = Easy
    | Medium
    | Hard


instance decodeJsonDifficulty :: DecodeJson Difficulty where
    decodeJson json = do
        str <- decodeJson json
        case String.toLower str of
            "easy" -> pure Easy
            "medium" -> pure Medium
            "hard" -> pure Hard
            _ -> Left $ UnexpectedValue json



data Question
    = Question
    { category          :: String
    , type              :: Type
    , difficulty        :: Difficulty
    , question          :: String
    , answers           :: Array Answer
    }

type Answer =
    { answer :: String
    , is_correct :: Boolean
    }

instance decodeJsonQuestion :: DecodeJson Question where
    decodeJson json = do
        obj <- decodeJson json
        category <- obj .: "category"
        t <- obj .: "type"
        question  <- obj .: "question"
        difficulty <- obj .: "difficulty"
        correct_answer <- obj .: "correct_answer"
        incorrect_answers <- obj .: "incorrect_answers"
        let
            answers =
                { answer: correct_answer, is_correct: true}
                : map (\a -> { answer: a, is_correct: false }) incorrect_answers
        pure $ Question { category, type: t, difficulty, question, answers }


shuffleAnswers :: forall m. MonadEffect m => Question -> m Question
shuffleAnswers (Question question) = liftEffect do
    shuffledAnswers <- shuffle question.answers
    pure $ Question $ question { answers = shuffledAnswers }
