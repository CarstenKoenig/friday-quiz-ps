module Components.Question where
  
import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..), isJust)
import Data.Question (Question(..), Answer)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), Component)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Html.Renderer.Halogen as RH
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent as ME


data Query a
    = Reset a
type Input = Question
data Output
    = AnswerGiven Answer
    | MoveNext


component :: forall m. MonadEffect m => Component HTML Query Input Output m
component = Hooks.component createHook
  where
  createHook { queryToken, outputToken } (Question question) = Hooks.do
    answered /\ answeredId <- Hooks.useState Nothing

    Hooks.useQuery queryToken case _ of
        Reset cont -> do
            Hooks.put answeredId Nothing
            pure $ Just cont

    Hooks.pure $ gameDiv answered answeredId
    where 
    gameDiv answered answeredId =
      HH.div
          [ HP.classes 
              [ ClassName "Question"
              ] 
          ]
          [ card ] 
      where
        onClickHandler answer _ = Just $ do
            Hooks.put answeredId (Just answer)
            Hooks.raise outputToken (AnswerGiven answer)

        gotoNextHandler event = Just do
            liftEffect $ preventDefault $ ME.toEvent event
            Hooks.raise outputToken MoveNext

        card =
            HH.div
                [ HP.class_ (ClassName $ if isJust answered then "card is-flipped" else "card") ]
                [ cardFront
                , cardBack
                ]
        cardFront =
            HH.div
                [ HP.class_ (ClassName "card-face front") ]
                [ HH.div_
                    [ HH.h1_ [ RH.render_ question.question ]
                    , answersList question.answers
                    ]
                ]
        cardBack =
            HH.div
                [ HP.class_ (ClassName "card-face back") ]
                ( case answered of 
                    Nothing -> []
                    Just givenAnswer ->
                        [ HH.div_
                            [ HH.h1_ [ RH.render_ question.question ]
                            , answersBackList $ filter (\a -> a.is_correct || a == givenAnswer) question.answers
                            , navigation
                            ]
                        ]
                )
        answersList answers =
            HH.div
                [ HP.class_ (ClassName "Answers") ]
                [ HH.ul_ (map answerItem answers) ]
        answerItem answer =
            HH.li
                [ HE.onClick (onClickHandler answer) ]
                [ RH.render_ answer.answer ]
        answersBackList answers =
            HH.div
                [ HP.class_ (ClassName "Answers") ]
                [ HH.ul_ (map answerBackItem answers) ]
        answerBackItem answer =
            HH.li
                [ HP.class_ $ ClassName $ if answer.is_correct then "correct" else "incorrect" ]
                [ RH.render_ answer.answer ]
        navigation =
            HH.div
                [ HP.class_ (ClassName "card-navigation") ]
                [ HH.a 
                    [ HP.href "#" 
                    , HE.onClick gotoNextHandler
                    ] 
                    [HH.text "next"] 
                ]