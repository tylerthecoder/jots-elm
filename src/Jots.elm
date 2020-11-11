module Jots exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, string, int, map3, list)
import Json.Encode
import Http
import Time
import Array exposing (Array)
import Debug
import Set

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

type alias Jot =
  { id : String
  , data : String
  , time : Int
  , tag : Maybe String
  }

type RequestState
  = Waiting
  | Loading
  | Failure Http.Error
  | Success

type JotTagFilter
  = NoTagFilter
  | TagFilter String

type JotTextFilter
  = NoTextFilter
  | TextFilter String

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
  , jots: Maybe (List Jot)

-- Tag stuff
  , editingTag: String
  , tagText: String
  , setTagRequest: RequestState

  , tagFilter: JotTagFilter
  }


init: () -> (Model, Cmd Msg)
init _ =
  (Model EnteringPassword "" "" Waiting Waiting Nothing "" "" Waiting NoTagFilter, Cmd.none)

-- UPDATE

type Msg
  = GetEntries
  | GetJotsResponse (Result Http.Error (List Jot))
  | CreateJotResponse (Result Http.Error String)
  | SetTagResponse (Result Http.Error String)
  | Password String
  | JotText String
  | ShowJotForm
  | ShowJots
  | CreateJot
  | EditTag String
  | SetTag String
  | UpdateTag
  | SetTagFilter JotTagFilter

baseApiUrl : String
baseApiUrl = "http://api.tylertracy.com"
-- baseApiUrl = "http://localhost:3000"

getApiUrl: Model -> String -> String
getApiUrl model path =
  baseApiUrl ++ path ++ "?pwd=" ++ model.password


getJotsRequest: Model -> Cmd Msg
getJotsRequest model =
  Http.get
    { url= getApiUrl model "/jots"
    , expect = Http.expectJson GetJotsResponse noteDecoder
    }

createJotRequest: Model -> String -> Cmd Msg
createJotRequest model jotText =
  Http.post
  { url= getApiUrl model "/jot"
  , expect = Http.expectString CreateJotResponse
  , body = Http.jsonBody (createJotEncoder model.jotText)
  }

setTagRequest: Model -> String -> String -> Cmd Msg
setTagRequest model jotId tag =
  Http.post
  { url = getApiUrl model ("/jot/" ++ jotId ++ "/tag")
  , expect = Http.expectString SetTagResponse
  , body = Http.jsonBody (tagJotEncoder model.tagText)
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetJotsResponse result ->
      case result of
        Ok jots ->
          ({ model
           | getJotRequest = Success
           , jots = Just jots
           , appState = ViewingJots
           }, Cmd.none)
        Err error ->
          ( {model | getJotRequest = Failure error}, Cmd.none)

    CreateJotResponse result ->
      case result of
        Ok _ ->
          ({model | createJotRequest = Success, appState = ViewingJots}, Cmd.none)
        Err errorText ->
          ({model | createJotRequest = Failure errorText}, Cmd.none)

    SetTagResponse result ->
      case result of
        Ok _ ->
          ({model
           | setTagRequest = Success
           , editingTag = ""
           , tagText = ""
           , jots = setTagInList model.jots model.editingTag model.tagText
           }, Cmd.none)
        Err errorText ->
          ({model | setTagRequest = Failure errorText}, Cmd.none)

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

    -- Tag Stuff
    EditTag jotId ->
      ({ model | editingTag = jotId, tagText = "" }, Cmd.none)

    SetTag tagText ->
      ({ model | tagText = tagText}, Cmd.none)

    UpdateTag ->
      ( model
      , setTagRequest model model.editingTag model.tagText )

    SetTagFilter tag ->
      ({ model | tagFilter = tag }, Cmd.none )


setTagIfSameId: String -> String -> Jot -> Jot
setTagIfSameId jotId tag jot =
  if jot.id == jotId then
    { jot | tag = Just tag}
  else
    jot

setTagInList: (Maybe (List Jot)) -> String -> String -> (Maybe (List Jot))
setTagInList maybeJots jotId tag =
  case maybeJots of
     Just jots -> Maybe.Just(List.map (setTagIfSameId jotId tag) jots)
     Nothing -> Maybe.Nothing

removeNothingFromList : List (Maybe a) -> List a
removeNothingFromList list =
    List.filterMap identity list

getUniqueTags: List Jot -> List String
getUniqueTags jots =
  jots
    |> List.map .tag
    |> removeNothingFromList
    |> Set.fromList
    |> Set.toList


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
      ViewingJots -> viewMain model
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

viewMain: Model -> Html Msg
viewMain model =
  case model.jots of
    Just jots ->
      div [ style "display" "flex" ]
        [ viewTagPanel model jots
        , div []
          [ div [] (viewJots model jots)
          , br [] []
          ]
        , viewCreateJotButton model
        ]
    Nothing -> text ""

viewTagPanel: Model -> List Jot -> Html Msg
viewTagPanel model jots =
  div []
  [ h1 [] [ text "Tags" ]
  , viewTagButton NoTagFilter
  , div []
    ( jots
      |> getUniqueTags
      |> List.map (\tag -> viewTagButton (TagFilter tag))
      )
  ]

viewTagButton: JotTagFilter -> Html Msg
viewTagButton tag =
    div
      [ onClick (SetTagFilter tag)
      , style "cursor" "pointer"
      ]
      [ case tag of
          NoTagFilter -> text "No Filter"
          TagFilter tagText -> text tagText
      ]


filterJots: JotTagFilter -> Jot -> Bool
filterJots filter jot =
  case filter of
    NoTagFilter -> True
    TagFilter tag -> case jot.tag of
      Just jotTag -> jotTag == tag
      Nothing -> False

viewJots: Model -> List Jot -> List (Html Msg)
viewJots model jots =
  jots
    |> List.sortBy .time
    |> List.filter (filterJots model.tagFilter)
    |> List.map (viewJot model)

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

buttonStyle =
  [ style "border" "none"
  , style "background-color" "#c33"
  , style "margin" "auto"
  ]

viewPasswordInput: Model -> Html Msg
viewPasswordInput model =
  div centerBoxStyle
    [ h1 [ align "center" ] [text "JOT"]
    , input [ type_ "text", placeholder "Enter Password", value model.password, onInput Password ] []
    , br [] []
    , button
      [ onClick GetEntries ]
      [ text "Enter" ]
    , br [] []
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

viewJot: Model -> Jot -> Html Msg
viewJot model jot =
  div jotStyle
    [ text jot.data
    , br [] []
    , if jot.id == model.editingTag then
        div []
          [ input [ value model.tagText, onInput SetTag ] []
          , button [ onClick (UpdateTag) ] [ text "Set Tag" ]
        ]
      else
        case jot.tag of
          Just tag -> text tag
          Nothing -> button [ onClick (EditTag jot.id)] [text "Add Tag"]
    , br [] []
    , text ("Date: " ++ (viewDate (Time.millisToPosix jot.time)))
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
    , input [ type_ "text", placeholder "tag" ] []
    , button [ onClick CreateJot ] [text "Create Jot"]
    , br [] []
    , button [ onClick ShowJots] [text "Cancel"]
    ]



noteDecoder: Decoder (List Jot)
noteDecoder =
  field "jots" (Json.Decode.list entryDecoder)


entryDecoder: Decoder Jot
entryDecoder =
  Json.Decode.map4 Jot
    (field "_id" string)
    (field "data" string)
    (field "time" int)
    (Json.Decode.maybe (field "tag" string))

createJotEncoder: String -> Json.Encode.Value
createJotEncoder jotText =
  Json.Encode.object
    [ ("text", Json.Encode.string jotText)
    ]

tagJotEncoder: String -> Json.Encode.Value
tagJotEncoder tag =
  Json.Encode.object
    [ ("tag", Json.Encode.string tag)
    ]