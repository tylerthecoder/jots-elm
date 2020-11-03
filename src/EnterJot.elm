module EnterJot exposing (..)
import Browser



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { os : ValueType1
    , key2 : ValueType2
    }




view: Model -> Html Msg