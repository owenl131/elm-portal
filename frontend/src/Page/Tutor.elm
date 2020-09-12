module Page.Tutor exposing (..)

import Browser.Navigation
import Element exposing (Element)
import RemoteData exposing (WebData)
import Tutor exposing (Tutor)


type alias Model =
    { key : Browser.Navigation.Key
    , id : String
    , data : WebData Tutor
    }


type Msg
    = DoNothing


init : Browser.Navigation.Key -> String -> ( Model, Cmd Msg )
init key id =
    ( { key = key, id = id, data = RemoteData.Loading }, Cmd.none )


view : Model -> Element Msg
view model =
    Element.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )
