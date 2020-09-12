module Page.Home exposing (..)

import Browser.Navigation as Navigation
import Element exposing (Element)

type alias Model =
    { key : Navigation.Key
    }


type Msg
    = Nothing


init : Navigation.Key -> Model
init key =
    { key = key }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    Element.text "Home"
