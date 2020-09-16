module Page.Class exposing (..)

import Browser.Navigation as Navigation
import Class exposing (Class)
import DatePicker exposing (Model)
import Element exposing (Element)
import RemoteData exposing (WebData)


type alias Model =
    { key : Navigation.Key
    , id : Int
    , data : WebData Class
    }


type Msg
    = DoNothing


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.data |> Maybe.map .name |> Maybe.withDefault ("Class ID: " ++ String.fromInt model.id)


getPageLink : Model -> String
getPageLink model =
    "/class/" ++ String.fromInt model.id


init : Int -> Navigation.Key -> ( Model, Cmd Msg )
init id key =
    ( { key = key, id = id, data = RemoteData.Loading }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    -- Should show details, menu to make new and sessions
    -- click session to redirect to take/view attendance page
    -- Button to open add tutor menu
    Element.text "Class"
