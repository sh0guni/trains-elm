module TrainApp where

import Html exposing (..)
import Json.Decode exposing (..)

type alias Model = Train

type alias Train =
  { trainNumber: Int,
  departureDate: String
  --,
  --operatorUICCode: Int,
  --operatorShortCode: String,
  --trainType: String,
  --trainCategory: String,
  --runningCurrently: Bool,
  --cancelled: Bool,
  --version: Int
  }

exampleTrainJsonString = "{  \"trainNumber\": 1,  \"departureDate\": \"2015-03-01\",  \"operatorUICCode\": 10,  \"operatorShortCode\": \"vr\",  \"trainType\": \"IC\",  \"trainCategory\": \"Long-distance\",  \"runningCurrently\": false,  \"cancelled\": false,  \"version\": 3467158366,  \"timeTableRows\": [    {      \"stationShortCode\": \"HKI\",      \"stationUICCode\": 1,      \"countryCode\": \"FI\",      \"type\": \"DEPARTURE\",      \"trainStopping\": true,      \"commercialStop\": true,      \"commercialTrack\": \"6\",      \"cancelled\": false,      \"scheduledTime\": \"2015-03-01T05:12:00.000Z\"    },    {      \"stationShortCode\": \"PSL\",      \"stationUICCode\": 10,      \"countryCode\": \"FI\",      \"type\": \"ARRIVAL\",      \"trainStopping\": true,      \"commercialStop\": true,      \"commercialTrack\": \"3\",      \"cancelled\": false,      \"scheduledTime\": \"2015-03-01T05:17:00.000Z\"    }]}"


exampleTrain : String -> Train
exampleTrain trainJsonString =
  decodeString trainDecoder trainJsonString
  |> Result.withDefault exampleTrain2

trainDecoder : Decoder Train
trainDecoder =
  object2 Train
    ("trainNumber" := int)
    ("departureDate" := string)

exampleTrain2 : Train
exampleTrain2 =
  { trainNumber= 0,
  departureDate = "xx"
  --operatorUICCode = 10,
  --operatorShortCode = "vr",
  --trainType = "IC",
  --trainCategory = "Long-distance",
  --runningCurrently = False,
  --cancelled = False,
  --version = 3467158366
  }

init : Model
init = exampleTrain exampleTrainJsonString

type Action
  = Search

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ text ("TrainNumber: " ++ toString model.trainNumber)]
    , div [] [ text ("Departure date: " ++ toString model.departureDate)]
    ]

update : Action -> Model -> Model
update action model = exampleTrain exampleTrainJsonString