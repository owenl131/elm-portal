module Page.Home exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Navigation
import Class exposing (Class)
import Element exposing (Element)
import RemoteData exposing (WebData)


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , classesToday : WebData (List Class)
    , myClasses : WebData (List Class)
    }


type alias Msg =
    ()


init : Api.Credentials -> Navigation.Key -> Model
init credentials key =
    { key = key
    , credentials = credentials
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
            , "Tutor extended fields, view fields by selection across tutors"
            , "Extended fields : languages spoken, days available, subjects keen, career goal, date of graduation, school type, remarks"
            , "Update class fields"
            , "Create class session"
            , "Implement auth"
            , "Implement CIP hours page"
            , "Implement sort by which field"
            , "Separate API out into its own file"
            , "Possibly set up a mock API for demo without server"
            , "|"
            , "This page would show user's classes and classes happening today"
            ]
        )
