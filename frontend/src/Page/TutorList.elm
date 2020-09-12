module Page.TutorList exposing (..)

import Browser.Navigation
import Element exposing (Element)
import Element.Input as Input
import RemoteData exposing (WebData)
import Time
import Tutor exposing (Gender, Tutor, TutorStatus)


type alias Pagination =
    { page : Int
    , perPage : Int
    , lastPage : Int
    }


defaultPagination : Pagination
defaultPagination =
    { page = 0
    , perPage = 20
    , lastPage = 0
    }


type TutorFilter
    = FilterStatus (List TutorStatus)
    | FilterGender (List Gender)
    | FilterName String
    | FilterSchool String
    | FilterDateOfBirth (Maybe Time.Posix) (Maybe Time.Posix)
    | FilterJoinDate (Maybe Time.Posix) (Maybe Time.Posix)


type alias Model =
    { key : Browser.Navigation.Key
    , pagination : Pagination
    , filters : List TutorFilter
    , data : WebData (List Tutor)
    }


type Msg
    = ChangePagePrevious
    | ChangePageNext
    | ChangePage Int


init : Browser.Navigation.Key -> Model
init key =
    { key = key
    , pagination = defaultPagination
    , filters = []
    , data = RemoteData.Loading
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Change page and load new data
        ChangePagePrevious -> ( model, Cmd.none )
        ChangePageNext -> ( model, Cmd.none )
        ChangePage page -> ( model, Cmd.none )



viewFilters : List TutorFilter -> Element Msg
viewFilters filters =
    Element.none
    -- Display add filter form
    -- Display existing filters


viewPagination : Pagination -> Element Msg
viewPagination pagination =
    Element.row
        []
        [ Input.button [] { onPress = Just ChangePagePrevious, label = Element.text "<" }
        , Input.button [] { onPress = Just (ChangePage 1), label = Element.text "1" }
        , Input.button [] { onPress = Just ChangePageNext, label = Element.text ">" }
        ]


viewData : WebData (List Tutor) -> Element Msg
viewData data = 
    case data of 
        RemoteData.NotAsked -> Element.text "Not asked"
        RemoteData.Loading -> Element.text "Loading"
        RemoteData.Failure err -> Element.text (Debug.toString err)
        RemoteData.Success tutorList -> Element.table
            []
            { columns = [
                { header = Element.text "Name"
                , width = Element.fill 
                , view = .name >> Element.text }
            ], data = tutorList }



view : Model -> Element Msg
view model =
    Element.column 
        []
        [ viewFilters model.filters
        , viewPagination model.pagination
        , viewData model.data
        , viewPagination model.pagination 
        ]
