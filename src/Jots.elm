module Jots exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, string, int, map2, list)
import Json.Encode
import Http
import Time
import Array exposing (Array)
import Debug

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

type alias NoteEntry =
  { data : String
  , time : Int
  }

type RequestState
  = Waiting
  | Loading
  | Failure Http.Error
  | Success



type AppState
  = EnteringPassword
  | ViewingJots
  | CreatingJot

type alias Model =
  { appState: AppState
  , password: String
  , jotText: String
  , getJotRequest: RequestState
  , createJotRequest: RequestState
  , jots: Maybe (List NoteEntry)
  }


init: () -> (Model, Cmd Msg)
init _ =
  (Model EnteringPassword "" "" Waiting Waiting Nothing, Cmd.none)

-- UPDATE

type Msg
  = GetEntries
  | GotEntries (Result Http.Error (List NoteEntry))
  | GotCreateJots (Result Http.Error String)
  | Password String
  | JotText String
  | ShowJotForm
  | ShowJots
  | CreateJot

getApiUrl: Model -> String -> String
getApiUrl model path =
  "http://localhost:3000" ++ path ++ "?pwd=" ++ model.password


getJotsRequest: Model -> Cmd Msg
getJotsRequest model =
  Http.get
    { url= getApiUrl model "/jots"
    , expect = Http.expectJson GotEntries noteDecoder
    }

createJotRequest: Model -> String -> Cmd Msg
createJotRequest model jotText =
  Http.post
  { url= getApiUrl model "/jot"
  , expect = Http.expectString GotCreateJots
  , body = Http.jsonBody (createJotEncoder model.jotText)
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotEntries result ->
      case result of
        Ok jots ->
          ({ model
           | getJotRequest = Success
           , jots = Just jots
           , appState = ViewingJots
           }, Cmd.none)
        Err error ->
          ( {model | getJotRequest = Failure error}, Cmd.none)

    GotCreateJots result ->
      case result of
        Ok _ ->
          ({model | createJotRequest = Success, appState = ViewingJots}, Cmd.none)
        Err errorText ->
          ({model | createJotRequest = Failure errorText}, Cmd.none)

    GetEntries ->
      ( { model | getJotRequest = Loading }
      , getJotsRequest model
      )

    Password password ->
      ({ model | password = password }, Cmd.none)

    JotText jotText ->
      ({ model | jotText=jotText }, Cmd.none)

    ShowJotForm ->
      ({ model | appState = CreatingJot }, Cmd.none)

    ShowJots ->
      ({ model | appState = ViewingJots }, Cmd.none)

    CreateJot ->
      ({ model | createJotRequest = Loading }
      , createJotRequest model model.jotText)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view: Model -> Html Msg
view model =
  div []
    [ case model.appState of
      EnteringPassword -> viewPasswordInput model
      ViewingJots -> viewJots model
      CreatingJot -> viewCreateJot model
    ]


viewCreateJotButton: Model -> Html Msg
viewCreateJotButton model =
  button
    [ style "position" "fixed"
    , style "bottom" "30px"
    , style "right" "30px"
    , style "border-radius" "50%"
    , style "backgroundColor" "red"
    , style "color" "white"
    , style "border" "1px solid white"
    , style "width" "70px"
    , style "height" "70px"
    , style "font-size" "3em"
    , style "cursor" "pointer"
    , onClick ShowJotForm
    ]
    [ text "+" ]

viewJots: Model -> Html Msg
viewJots model =
  case model.jots of
    Just jots ->
      div []
        [ div []
            ( List.map viewJot (List.sortBy .time jots))
        , viewCreateJotButton model
        ]
    Nothing -> text ""

centerBoxStyle: List (Attribute msg)
centerBoxStyle =
  [ style "position" "absolute"
  , style "top" "50%"
  , style "left" "50%"
  , style "backgroundColor" "white"
  , style "transform" "translate(-50%, -50%)"
  , style "border" "1px solid black"
  , style "border-radius" "10px"
  , style "padding" "10px"
  ]


viewPasswordInput: Model -> Html Msg
viewPasswordInput model =
  div centerBoxStyle
    [ text "JOT"
    , input [ type_ "text", placeholder "Enter Password", value model.password, onInput Password ] []
    , button [ onClick GetEntries ] [ text "Enter" ]
    , case model.getJotRequest of
        Waiting -> text ""
        Loading -> text "Loading"
        Failure err -> text "There was an error"
        Success -> text "success"
    ]


jotStyle : List (Attribute msg)
jotStyle =
    [ style "border-radius" "5px"
    , style "background-color" "#f2f2f2"
    , style "padding" "20px"
    , style "margin" "10px"
    , style "border" "2px solid black"
    ]

viewJot: NoteEntry -> Html Msg
viewJot entry =
  div jotStyle
    [ text entry.data
    , br [] []
    , text ("Date: " ++ (viewDate (Time.millisToPosix entry.time)))
    ]

viewDate: Time.Posix -> String
viewDate posix =
  (toMonthString (Time.toMonth Time.utc posix))
    ++ " "
    ++ String.fromInt (Time.toDay Time.utc posix)
    ++ " "
    ++ String.fromInt (Time.toYear Time.utc posix)

toMonthString : Time.Month -> String
toMonthString month =
  case month of
    Time.Jan -> "January"
    Time.Feb -> "Febuary"
    Time.Mar -> "March"
    Time.Apr -> "April"
    Time.May -> "May"
    Time.Jun -> "June"
    Time.Jul -> "July"
    Time.Aug -> "August"
    Time.Sep -> "September"
    Time.Oct -> "October"
    Time.Nov -> "November"
    Time.Dec -> "December"


viewCreateJot: Model -> Html Msg
viewCreateJot model =
  div centerBoxStyle
    [ input [ type_ "text", onInput JotText ] []
    , br [] []
    , button [ onClick CreateJot ] [text "Create Jot"]
    , br [] []
    , button [ onClick ShowJots] [text "Cancel"]
    ]



noteDecoder: Decoder (List NoteEntry)
noteDecoder =
  field "jots" (Json.Decode.list entryDecoder)


entryDecoder: Decoder NoteEntry
entryDecoder =
  map2
    NoteEntry
      (field "data" string)
      (field "time" int)

createJotEncoder: String -> Json.Encode.Value
createJotEncoder jotText =
  Json.Encode.object
    [ ("text", Json.Encode.string jotText)
    ]

