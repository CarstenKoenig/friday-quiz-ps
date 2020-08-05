module Api.Question
( getQuestions
, FetchError (..)
) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Question (Question, shuffleQuestions)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)


data FetchError
    = HttpError String
    | JsonError String


instance showFetchError :: Show FetchError where
    show (HttpError err) = "HTTP-Error: " <> err
    show (JsonError err) = "JSON-Error: " <> err


getQuestions :: forall m. MonadAff m => MonadEffect m => String -> m (Either FetchError (Array Question))
getQuestions url = do
    result <- liftAff $ AX.get ResponseFormat.json url
    case process (lmap (HttpError <<< AX.printError) result) of
        Left err ->  pure $ Left err
        Right questions -> Right <$> shuffleQuestions questions
    where
    process result = do
        json <- _.body <$> result
        questions <- lmap (JsonError <<< printJsonDecodeError)
            $ (decodeJson json :: Either JsonDecodeError QuestionsJson)
        pure questions.results


type QuestionsJson =
    { results :: Array Question
    }

