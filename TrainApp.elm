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
  { train: Maybe Train
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

emptyTrain : Train
emptyTrain =
  { trainNumber = 0
  , departureDate = ""
  , timeTableRows = []
  }

init : (Model, Effects Action)
init = 
  ( { train = Nothing
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
      ( { model | train = maybeTrain }
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

trainInfo : Maybe Train -> Html
trainInfo maybeTrain =
  case maybeTrain of
    Just train ->
      div []
        [ div [] [ text ("TrainNumber: " ++ toString train.trainNumber) ]
        , div [] [ text ("Departure date: " ++ train.departureDate) ]
        , div [] [ text "Time table:", timeTable train.timeTableRows ]
        ]

    Nothing ->
      div [] [ text "Click Search to see train timetable" ]

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
          [ placeholder model.departureDate
          , on "input" targetValue
            (\string -> Signal.message address
                (DepartureDate string))
          ] []
        ]
    , div []
        [ button [ onClick address Search ] [ text "Search" ]]
      ]
