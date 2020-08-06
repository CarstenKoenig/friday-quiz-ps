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
import Data.Questions (Questions, shuffleQuestions)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)


data FetchError
    = HttpError String
    | JsonError String


instance showFetchError :: Show FetchError where
    show (HttpError err) = "HTTP-Error: " <> err
    show (JsonError err) = "JSON-Error: " <> err


getQuestions :: forall m. MonadAff m => MonadEffect m => String -> m (Either FetchError Questions)
getQuestions url = do
    result <- liftAff $ AX.get ResponseFormat.json url
    case process (lmap (HttpError <<< AX.printError) result) of
        Left err ->  pure $ Left err
        Right questions -> Right <$> shuffleQuestions questions
    where
    process result = do
        json <- _.body <$> result
        lmap (JsonError <<< printJsonDecodeError)
            $ (decodeJson json :: Either JsonDecodeError Questions)
