module Page.Class.Attendance exposing
    ( Model
    , Msg
    , getPageLink
    , getPageTitle
    , init
    , update
    , view
    )

import Browser.Navigation as Navigation
import Class
import Element exposing (Element)
import Http
import RemoteData exposing (WebData)
import Tutor exposing (Tutor)


type alias Model =
    { key : Navigation.Key
    , classId : Int
    , sessionId : Int
    , sessionData : WebData Class.ClassSession
    , tutors : WebData (List Tutor)
    , present : WebData (List String)
    }


type Msg
    = GotTutorsList (Result Http.Error (List Tutor))
    | GotPresentList (Result Http.Error (List String))
    | GotSessionData (Result Http.Error Class.ClassSession)
    | MarkPresent String
    | MarkAbsent String
    | AddExternalTutor String
    | RemoveExternalTutor String


getPageTitle : Model -> String
getPageTitle model =
    "Take Attendance"


getPageLink : Model -> String
getPageLink model =
    "/class/"
        ++ String.fromInt model.classId
        ++ "/session/"
        ++ String.fromInt model.sessionId


init : Navigation.Key -> Int -> Int -> Model
init key classId sessionId =
    { key = key
    , classId = classId
    , sessionId = sessionId
    , sessionData = RemoteData.Loading
    , present = RemoteData.Loading
    , tutors = RemoteData.Loading
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    Element.text "Take attendance"
