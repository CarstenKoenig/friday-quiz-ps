
module Components.Game where
  
import Prelude

import Components.Question as Q
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Question as Qu
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
  createHook { slotToken } qs = Hooks.do
    questions /\ questionsId <- Hooks.useState qs
    currentQuestionIndex /\ currentQuestionIndexId <- Hooks.useState 0
    let currentQuestion = questions Qs.!! currentQuestionIndex

    Hooks.pure $ gameDiv currentQuestion currentQuestionIndexId questions questionsId
    where 
    gameDiv currentQuestion currentQuestionIndexId questions questionsId =
      HH.div
          [ HP.classes 
              [ ClassName "Game"
              ] 
          ]
          [ displayCurrentQuestion
          , displayState (Qs.getQuestionStates questions)
          ]
      where
      displayCurrentQuestion =
          case currentQuestion of
              Nothing -> HH.text ""
              Just question ->
                HH.slot _question unit Q.component question handleQuestionOutput

      displayState states =
        HH.div
          [ HP.class_ (ClassName "States") ]
          (map showState states)
        where
        showState Qu.NotAnswered =
          HH.span
            [ HP.class_ (ClassName "not-answered") ]
            [ HH.text "○"]
        showState Qu.AnsweredCorrect =
          HH.span
            [ HP.class_ (ClassName "correct-answer") ]
            [ HH.text "●"]
        showState Qu.AnsweredWrong =
          HH.span
            [ HP.class_ (ClassName "wrong-answer") ]
            [ HH.text "●"]

      handleQuestionOutput output =
        case output of
          Q.MoveNext -> Just do
            ind <- Hooks.modify currentQuestionIndexId (\i -> (i+1) `mod` Qs.size questions)
            question <- (_ Qs.!! ind) <$> Hooks.get questionsId
            case question of
              Just q ->
                void $ Hooks.query slotToken _question unit $ H.tell (Q.Reset q)
              Nothing ->
                pure unit
          Q.AnswerGiven answer -> Just do
            ind <- Hooks.get currentQuestionIndexId
            Hooks.modify_ questionsId (Qs.setAnswerAt ind answer)