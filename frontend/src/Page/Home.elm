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
    Element.column
        [ Element.spacing 5 ]
        [ Element.text "Home - Use this for short term dev goals"
        , Element.text "Update tutor fields"
        , Element.text "Get and parse pagination set up from server"
        , Element.text "Implement class in front and backend"
        , Element.text "Implement attendance in front and backend"
        ]
