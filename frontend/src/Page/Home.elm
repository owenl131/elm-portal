module Page.Home exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Class exposing (Class)
import Element exposing (Element)
import RemoteData exposing (WebData)


type alias Model =
    { key : Navigation.Key
    , classesToday : WebData (List Class)
    , myClasses : WebData (List Class)
    }


type alias Msg =
    ()


init : Navigation.Key -> Model
init key =
    { key = key
    , classesToday = RemoteData.NotAsked
    , myClasses = RemoteData.NotAsked
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Element Msg
view _ =
    Element.column
        [ Element.spacing 5 ]
        (List.map Element.text
            [ "Home - Use this for short term dev goals"
            , "Update tutor fields"
            , "Implement auth"
            , "Get and parse pagination set up from server"
            , "Implement class in front and backend"
            , "Implement attendance in front and backend"
            ]
        )
