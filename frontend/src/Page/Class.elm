module Page.Class exposing (Model, Msg, getPageLink, getPageTitle, init, update, view)

import Browser.Navigation as Navigation
import Class exposing (Class)
import DatePicker exposing (Model)
import Element exposing (Element)
import Http
import RemoteData exposing (WebData)


type alias Model =
    { key : Navigation.Key
    , id : Int
    , data : WebData Class
    }


type Msg
    = GotClassData (Result Http.Error Class)


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.data |> Maybe.map .name |> Maybe.withDefault ("Class ID: " ++ String.fromInt model.id)


getPageLink : Model -> String
getPageLink model =
    "/class/" ++ String.fromInt model.id


fetchClassData : Model -> Cmd Msg
fetchClassData model =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt model.id
        , expect = Http.expectJson GotClassData Class.classDecoder
        }


init : Int -> Navigation.Key -> ( Model, Cmd Msg )
init id key =
    let
        model =
            { key = key, id = id, data = RemoteData.Loading }
    in
    ( model, fetchClassData model )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotClassData result ->
            ( { model | data = RemoteData.fromResult result }, Cmd.none )


view : Model -> Element Msg
view _ =
    -- Should show details, menu to make new and sessions
    -- click session to redirect to take/view attendance page
    -- Button to open add tutor menu
    Element.text "Class"
