module Page.Class.AddTutor exposing (..)

import Browser.Navigation as Navigation
import Date
import DatePicker
import Element exposing (Element)
import Http
import RemoteData exposing (WebData)
import Tutor exposing (Tutor)


type alias Model =
    { key : Navigation.Key
    , id : Int
    , tutors : WebData (List Tutor)
    , nameFilter : String
    , joinDate : Maybe Date.Date
    , joinDatePicker : DatePicker.Model
    , suggestions : WebData (List Tutor)
    }


type Msg
    = GotTutorSuggestionList (Result Http.Error (List Tutor))
    | GotTutorList (Result Http.Error (List Tutor))
    | EnteredNameFilter String


getPageTitle : Model -> String
getPageTitle _ =
    "Add Tutors"


getPageLink : Model -> String
getPageLink model =
    "/class/" ++ String.fromInt model.id ++ "/addtutor"


init : Navigation.Key -> Int -> ( Model, Cmd Msg )
init key id =
    ( { key = key
      , id = id
      , tutors = RemoteData.Loading
      , nameFilter = ""
      , suggestions = RemoteData.Loading
      , joinDate = Nothing
      , joinDatePicker = DatePicker.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


viewSelector : Model -> Element Msg
viewSelector model =
    Element.text "menu"


viewList : Model -> Element Msg
viewList model =
    Element.text "List"


view : Model -> Element Msg
view model =
    Element.row
        [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.el
            [ Element.height Element.fill, Element.width Element.fill ]
            (viewSelector model)
        , Element.el
            [ Element.height Element.fill, Element.width Element.fill ]
            (viewList model)
        ]
