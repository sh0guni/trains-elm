module TrainApp where

import Html exposing (..)

type alias Model =
  { trainNumber: Int,
  departureDate: String,
  operatorUICCode: Int,
  operatorShortCode: String,
  trainType: String,
  trainCategory: String,
  runningCurrently: Bool,
  cancelled: Bool,
  version: Int
  }

exampleTrain : Model
exampleTrain =
  { trainNumber= 1,
  departureDate = "2015-03-01",
  operatorUICCode = 10,
  operatorShortCode = "vr",
  trainType = "IC",
  trainCategory = "Long-distance",
  runningCurrently = False,
  cancelled = False,
  version = 3467158366
  }

init : Model
init = exampleTrain

type Action
  = Search

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ text ("TrainNumber: " ++ toString model.trainNumber)]
    , div [] [ text ("Departure date: " ++ toString model.departureDate)]
    ]

update : Action -> Model -> Model
update action model = exampleTrain