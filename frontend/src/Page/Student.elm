module Page.Student exposing (..)

import Api exposing (Credentials)
import Browser.Navigation as Navigation
import Element exposing (Element)
import Student exposing (StudentId)


type Msg
    = SampleMsg


type alias Model =
    { id : StudentId
    , credentials : Api.Credentials
    , key : Navigation.Key
    }


init : Api.Credentials -> Navigation.Key -> StudentId -> ( Model, Cmd Msg )
init credentials key id =
    ( { id = id
      , credentials = credentials
      , key = key
      }
    , Cmd.none
    )


getPageTitle : Model -> String
getPageTitle model =
    ""


getPageLink : StudentId -> String
getPageLink model =
    ""


view : Model -> Element Msg
view model =
    Element.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
