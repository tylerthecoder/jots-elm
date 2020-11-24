port module Jots exposing (..)

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

main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port setPassword: String -> Cmd msg

-- MODEL

type alias Jot =
  { id : String
  , data : String
  , time : Int
  , tag : Maybe String
  }

type alias CreateJotData =
  { text: String
  , tag: String
  }

type CreateJotUpdate
  = CreateJotText String
  | CreateJotTag String

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
  , getJotRequest: RequestState
  , createJotRequest: RequestState
  , jots: Maybe (List Jot)

  , showTagPanel: Bool

-- Create Jot Form Data
  , createJotData: CreateJotData

-- Tag stuff
  , editingTag: String
  , tagText: String
  , setTagRequest: RequestState

  , tagFilter: JotTagFilter
  }

emptyModel: Model
emptyModel =
  { appState = EnteringPassword
  , password = ""
  , getJotRequest = Waiting
  , createJotRequest = Waiting
  , showTagPanel = True
  , jots = Nothing
  , createJotData = { text="", tag="" }
  , editingTag = ""
  , tagText = ""
  , setTagRequest = Waiting
  , tagFilter = NoTagFilter
  }


-- This will be passed the password if it is in the local storage
init: Maybe String -> (Model, Cmd Msg)
init maybePassword =
  case maybePassword of
    Just password ->
      let
          newModel = { emptyModel | password = password }
      in
        (newModel, getJotsRequest newModel)
    Nothing ->
      (emptyModel, Cmd.none)


-- Netowrk Commands

baseApiUrl : String
baseApiUrl = "https://api.tylertracy.com"
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

createJotRequest: Model -> Cmd Msg
createJotRequest model =
  Http.post
  { url= getApiUrl model "/jot"
  , expect = Http.expectString CreateJotResponse
  , body = Http.jsonBody (createJotEncoder model.createJotData.text model.createJotData.tag)
  }

setTagRequest: Model -> String -> String -> Cmd Msg
setTagRequest model jotId tag =
  Http.post
  { url = getApiUrl model ("/jot/" ++ jotId ++ "/tag")
  , expect = Http.expectString SetTagResponse
  , body = Http.jsonBody (tagJotEncoder model.tagText)
  }

-- UPDATE

type Msg
  = GetEntries
  | GetJotsResponse (Result Http.Error (List Jot))
  | CreateJotResponse (Result Http.Error String)
  | SetTagResponse (Result Http.Error String)
  | Password String
  | UpdateCreateJotData CreateJotUpdate
  | ShowJotForm
  | ShowJots
  | CreateJot
  | EditTag String
  | SetTag String
  | UpdateTag
  | SetTagFilter JotTagFilter
  | ToggleTagPanel

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
           }, setPassword model.password)
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

    UpdateCreateJotData createJotUpdate ->
      let
        oldCreateJot = .createJotData model
        newCreateJot = (
          case createJotUpdate of
            CreateJotText text -> {oldCreateJot | text = text}
            CreateJotTag tag -> {oldCreateJot | tag = tag})
      in
        ({model | createJotData = newCreateJot }, Cmd.none)

    ShowJotForm ->
      ({ model | appState = CreatingJot }, Cmd.none)

    ShowJots ->
      ({ model | appState = ViewingJots }, Cmd.none)

    CreateJot ->
      ({ model | createJotRequest = Loading }
      , createJotRequest model)

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

    ToggleTagPanel ->
      ({ model | showTagPanel = not model.showTagPanel}, Cmd.none)


-- PURE LOGIC

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

filterJots: JotTagFilter -> Jot -> Bool
filterJots filter jot =
  case filter of
    NoTagFilter -> True
    TagFilter tag -> case jot.tag of
      Just jotTag -> jotTag == tag
      Nothing -> False

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

viewMain: Model -> Html Msg
viewMain model =
  case model.jots of
    Just jots ->
      div [ style "display" "flex" ]
        [ viewMenuButton model
        , if model.showTagPanel then
          viewTagPanel model jots
        else
          text ""
        , viewJotsPanel model jots
        , viewCreateJotButton
        ]
    Nothing -> text ""

viewMenuButton: Model -> Html Msg
viewMenuButton model =
  div [] [
    i
    [ id "menuButton"
    , class "fas fa-bars"
    , onClick ToggleTagPanel
    ] []
  ]

viewCreateJotButton: Html Msg
viewCreateJotButton =
  button
    [ id "createJotButton"
    , onClick ShowJotForm
    ]
    [ text "+"
    ]

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

viewJotsPanel: Model -> List Jot -> Html Msg
viewJotsPanel model jots =
  div
    [ id "jotsPanel"
    ]
    (viewJots model jots)

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

viewJots: Model -> List Jot -> List (Html Msg)
viewJots model jots =
  jots
    |> List.sortBy .time
    |> List.filter (filterJots model.tagFilter)
    |> List.map (viewJot model)

viewPasswordInput: Model -> Html Msg
viewPasswordInput model =
  div
    [ class "centerBox" ]
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

viewJot: Model -> Jot -> Html Msg
viewJot model jot =
  div
    [ class "jot" ]
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

viewCreateJot: Model -> Html Msg
viewCreateJot model =
  div
    [ class "centerBox" ]
    [ textarea [ placeholder "Jot", onInput (\s -> UpdateCreateJotData (CreateJotText s)) ] []
    , br [] []
    , input [ type_ "text", placeholder "tag", onInput (\s -> UpdateCreateJotData (CreateJotTag s)) ] []
    , br [] []
    , button [ onClick CreateJot ] [text "Create Jot"]
    , br [] []
    , button [ onClick ShowJots] [text "Cancel"]
    ]

-- JSON SHIT

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

createJotEncoder: String -> String -> Json.Encode.Value
createJotEncoder jotText jotTag =
  Json.Encode.object
    [ ("text", Json.Encode.string jotText)
    , ("tag", Json.Encode.string jotTag )
    ]

tagJotEncoder: String -> Json.Encode.Value
tagJotEncoder tag =
  Json.Encode.object
    [ ("tag", Json.Encode.string tag)
    ]