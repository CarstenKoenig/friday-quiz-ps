
module Components.Game where
  
import Prelude

import Components.Question as Q
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Questions (Questions)
import Data.Questions as Qs
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, Slot)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Query = Const Void
type Input = Questions
type Output = Void

-- Slots for Child-Components
type Slots = 
    ( question :: Slot Q.Query Q.Input Unit
    )

-- Proxy to help bridge value/type levels and address for slots
_question = SProxy :: SProxy "question"


component :: forall m. MonadEffect m => Component HTML Query Input Output m
component = Hooks.component createHook
  where
  createHook { slotToken } questions = Hooks.do
    currentQuestionIndex /\ currentQuestionIndexId <- Hooks.useState 0
    let currentQuestion = questions Qs.!! currentQuestionIndex

    Hooks.pure $ gameDiv currentQuestion currentQuestionIndexId
    where 
    gameDiv currentQuestion currentQuestionIndexId =
      HH.div
          [ HP.classes 
              [ ClassName "Game"
              ] 
          ]
          [ displayCurrentQuestion
          ]
      where
      displayCurrentQuestion =
          case currentQuestion of
              Nothing -> HH.text ""
              Just question ->
                HH.slot _question unit Q.component question handleQuestionOutput

      handleQuestionOutput output =
        case output of
          Q.MoveNext -> Just do
            void $ Hooks.query slotToken _question unit $ H.tell Q.Reset
            Hooks.modify_ currentQuestionIndexId (\i -> (i+1) `mod` Qs.size questions)
          _ -> Nothing