module TrainApp where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode exposing (..)
import Task
import String exposing (toInt)

type alias Model = 
  { train: Train
  , trainNumber: Int
  , departureDate: String
  }

type alias Train =
  { trainNumber: Int
  , departureDate: String
  , timeTableRows: List TimeTableRow
  }

type alias TimeTableRow =
  { stationShortCode: String
  , rowType: String
  , scheduledTime: String
  }

exampleTrainJsonString : String
exampleTrainJsonString = "{  \"trainNumber\": 1,  \"departureDate\": \"2015-03-01\",  \"operatorUICCode\": 10,  \"operatorShortCode\": \"vr\",  \"trainType\": \"IC\",  \"trainCategory\": \"Long-distance\",  \"runningCurrently\": false,  \"cancelled\": false,  \"version\": 3467158366,  \"timeTableRows\": [    {      \"stationShortCode\": \"HKI\",      \"stationUICCode\": 1,      \"countryCode\": \"FI\",      \"type\": \"DEPARTURE\",      \"trainStopping\": true,      \"commercialStop\": true,      \"commercialTrack\": \"6\",      \"cancelled\": false,      \"scheduledTime\": \"2015-03-01T05:12:00.000Z\"    },    {      \"stationShortCode\": \"PSL\",      \"stationUICCode\": 10,      \"countryCode\": \"FI\",      \"type\": \"ARRIVAL\",      \"trainStopping\": true,      \"commercialStop\": true,      \"commercialTrack\": \"3\",      \"cancelled\": false,      \"scheduledTime\": \"2015-03-01T05:17:00.000Z\"    }]}"

exampleTrain : String -> Train
exampleTrain trainJsonString =
  decodeString trainDecoder trainJsonString
  |> Result.withDefault exampleTrain2

trainDecoder : Decoder Train
trainDecoder =
  object3 Train
    ("trainNumber" := int)
    ("departureDate" := string)
    ("timeTableRows" := Json.Decode.list timeTableRowDecoder)

timeTableRowDecoder : Decoder TimeTableRow
timeTableRowDecoder =
  object3 TimeTableRow
    ("stationShortCode" := string)
    ("type" := string)
    ("scheduledTime" := string)

exampleTrain2 : Train
exampleTrain2 =
  { trainNumber= 0
  , departureDate = "xx"
  , timeTableRows = [exampleTimeTableRow]
  }

exampleTimeTableRow : TimeTableRow
exampleTimeTableRow = { stationShortCode = "HKI", rowType = "DEPARTURE", scheduledTime = "2015-03-01T05:12:00.000Z" }

init : (Model, Effects Action)
init = 
  ( { train = exampleTrain exampleTrainJsonString
    , trainNumber = 1
    , departureDate = "2015-03-01"
    }
  , Effects.none
  )

type Action
  = TrainNumber Int
  | DepartureDate String
  | Search
  | NewTrain (Maybe Train)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    TrainNumber trainNumber ->
      ( { model | trainNumber = trainNumber }
      , Effects.none
      )

    DepartureDate departureDate ->
      ( { model | departureDate = departureDate }
      , Effects.none
      )

    Search ->
      ( model
      , getTrain model
      )

    NewTrain maybeTrain ->
      let newTrain = Maybe.withDefault exampleTrain2 maybeTrain
      in
        ( { model | train = newTrain }
        , Effects.none
        )

getTrain : Model -> Effects Action
getTrain model =
  Http.get trainDecoder (getOpenDataURL model.trainNumber model.departureDate)
  |> Task.toMaybe
  |> Task.map NewTrain
  |> Effects.task

getOpenDataURL : Int -> String -> String
getOpenDataURL trainNumber departureDate =
  Http.url ("http://rata.digitraffic.fi/api/v1/schedules/" ++ (toString trainNumber))
    [ ("departure_date", departureDate) ]

view : Signal.Address Action -> Model -> Html
view address model =
  div []
  [ searchBox address model
  , trainInfo model.train
  ]

trainInfo : Train -> Html
trainInfo train =
  div []
    [ div [] [ text ("TrainNumber: " ++ toString train.trainNumber) ]
    , div [] [ text ("Departure date: " ++ toString train.departureDate) ]
    , div [] [ text "Time table:", timeTable train.timeTableRows ]
    ]

timeTable : List TimeTableRow -> Html
timeTable timeTableRows =
  table [ style [("textAlign", "left")] ]
    [ thead []
        [ tr [] 
           [ th [] [ text "Station" ]
           , th [] [ text "Type" ]
           , th [] [ text "Scheduled" ]
           ]
        ]
    , tbody [] (List.map timeTableRow timeTableRows)
    ]

timeTableRow : TimeTableRow -> Html
timeTableRow row =
  tr []
    [ td [] [ text row.stationShortCode ]
    , td [] [ text row.rowType ]
    , td [] [ text row.scheduledTime ]
    ]

searchBox : Signal.Address Action -> Model -> Html
searchBox address model =
  div []
    [ div []
      [ text "Train number: "
        , input 
            [ placeholder (toString model.trainNumber)
            , on "input" targetValue 
              (\string -> Signal.message address 
                  (TrainNumber (Result.withDefault 0 (toInt string))))
            ] []
        ]
    , div []
      [ text "Departure date: "
        , input
          [ placeholder (toString model.departureDate)
          , on "input" targetValue
          (\string -> Signal.message address
              (DepartureDate string))
          ] []
        ]
    , div []
        [ button [ onClick address Search ] [ text "Search" ]]
      ]
