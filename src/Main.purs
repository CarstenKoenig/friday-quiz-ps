module Main where

import Prelude

import Api.Question as Api
import Components.Game as Game
import Data.Either (Either(..), either)
import Data.Questions as Q
import Effect (Effect)
import Effect.Class.Console (log)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

liveUrl :: String
liveUrl = "https://opentdb.com/api.php?amount=10"

mockUrl :: String
mockUrl = "http://localhost:1235/Example.json"

main :: Effect Unit
main = do
  runHalogenAff do
    -- get a handle to the body after it's available
    body <- awaitBody
    -- get test-input
    questions <- do
      qs <- Api.getQuestions liveUrl
      case qs of
        Left (Api.JsonError err) -> log err
        Left (Api.HttpError err) -> log err
        _ -> pure unit
      pure $ either (const Q.empty) identity qs
    -- run the game-component
    -- into the body of the browser-document
    runUI Game.component questions body