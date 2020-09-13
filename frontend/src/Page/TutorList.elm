module Page.TutorList exposing (..)

import Browser.Navigation as Navigation
import Date
import DatePicker
import Element exposing (Element)
import Element.Input as Input
import Http
import Json.Decode as Decode
import Maybe.Extra
import RemoteData exposing (WebData)
import Task
import Time
import Tutor exposing (Gender, Tutor, TutorStatus, datestringEncoder, tutorDecoder)


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



-- | FilterClass String


type alias TutorFilters =
    { statuses : Maybe (List TutorStatus)
    , genders : Maybe (List Gender)
    , names : Maybe (List String)
    , schools : Maybe (List String)
    , dobLower : Maybe Date.Date
    , dobUpper : Maybe Date.Date
    , joinDateLower : Maybe Date.Date
    , joinDateUpper : Maybe Date.Date
    , classes : Maybe (List String)
    , joinLowerPicker : DatePicker.Model
    , joinUpperPicker : DatePicker.Model
    }


emptyTutorFilter : TutorFilters
emptyTutorFilter =
    { statuses = Nothing
    , genders = Nothing
    , names = Nothing
    , schools = Nothing
    , dobLower = Nothing
    , dobUpper = Nothing
    , joinDateLower = Nothing
    , joinDateUpper = Nothing
    , classes = Nothing
    , joinLowerPicker = DatePicker.init
    , joinUpperPicker = DatePicker.init
    }


type alias Model =
    { key : Navigation.Key
    , pagination : Pagination
    , filters : TutorFilters
    , data : WebData (List Tutor)
    , joinLowerDate : Maybe Date.Date
    , joinUpperDate : Maybe Date.Date
    }


type Msg
    = ChangePagePrevious
    | ChangePageNext
    | ChangePage Int
    | ChangePicker DatePicker.ChangeEvent
    | ToDetails String
    | GotTutorList (Result Http.Error (List Tutor))
    | EnteredNameFilter String
    | EnteredSchoolFilter String
    | SetToday Date.Date


init : Navigation.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , pagination = defaultPagination
      , filters = emptyTutorFilter
      , data = RemoteData.Loading
      , joinLowerDate = Maybe.Nothing
      , joinUpperDate = Maybe.Nothing
      }
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:5000/tutors"
            , expect = Http.expectJson GotTutorList <| Decode.list tutorDecoder
            }
        , Task.perform SetToday Date.today
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Change page and load new data
        ChangePagePrevious ->
            ( model, Cmd.none )

        ChangePageNext ->
            ( model, Cmd.none )

        ChangePage _ ->
            ( model, Cmd.none )

        GotTutorList result ->
            case result of
                Err error ->
                    ( { model | data = RemoteData.Failure error }, Cmd.none )

                Ok data ->
                    ( { model | data = RemoteData.Success data }, Cmd.none )

        ToDetails id ->
            ( model, Navigation.pushUrl model.key ("/tutor/" ++ id) )

        EnteredNameFilter _ ->
            ( model, Cmd.none )

        EnteredSchoolFilter _ ->
            ( model, Cmd.none )

        SetToday today ->
            let
                filters =
                    model.filters
            in
            ( { model
                | filters =
                    { filters | joinLowerPicker = filters.joinLowerPicker |> DatePicker.setToday today }
              }
            , Cmd.none
            )

        ChangePicker changeEvent ->
            let
                filters =
                    model.filters
            in
            case changeEvent of
                DatePicker.DateChanged date ->
                    ( { model
                        | filters =
                            { filters | joinDateLower = Just date }
                      }
                    , Cmd.none
                    )

                DatePicker.TextChanged text ->
                    ( { model
                        | filters =
                            { filters
                                | joinDateLower =
                                    Date.fromIsoString text
                                        |> Result.toMaybe
                                        |> Maybe.Extra.orElse filters.joinDateLower
                            }
                      }
                    , Cmd.none
                    )

                DatePicker.PickerChanged subMsg ->
                    ( { model
                        | filters =
                            { filters
                                | joinLowerPicker =
                                    filters.joinLowerPicker
                                        |> DatePicker.update subMsg
                            }
                      }
                    , Cmd.none
                    )


viewFilters : TutorFilters -> Element Msg
viewFilters filters =
    -- Display add filter form
    -- Display existing filters
    Element.row
        [ Element.width Element.fill
        ]
        [ Element.column
            [ Element.width Element.fill ]
            [ Element.row
                []
                [ Input.text
                    []
                    { label = Input.labelLeft [] (Element.text "Filter Name")
                    , onChange = EnteredNameFilter
                    , placeholder = Nothing
                    , text = "Some text"
                    }
                ]
            , Element.row
                []
                [ DatePicker.input
                    []
                    { onChange = ChangePicker
                    , selected = filters.joinDateLower
                    , label = Input.labelLeft [] (Element.text "Joined before")
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = Maybe.withDefault "No bound" (Maybe.map Date.toIsoString filters.joinDateLower)
                    , model = filters.joinLowerPicker
                    }
                ]
            ]
        , Element.column
            []
            [ Element.text "Filters:" ]
        ]


viewPagination : Pagination -> Element Msg
viewPagination pagination =
    Element.row
        [ Element.width Element.fill ]
        [ Input.button [] { onPress = Just ChangePagePrevious, label = Element.text "<" }
        , Input.button [] { onPress = Just (ChangePage 1), label = Element.text "1" }
        , Input.button [] { onPress = Just ChangePageNext, label = Element.text ">" }
        ]


viewData : WebData (List Tutor) -> Element Msg
viewData data =
    case data of
        RemoteData.NotAsked ->
            Element.text "Not asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure err ->
            Element.text (Debug.toString err)

        RemoteData.Success tutorList ->
            Element.table
                []
                { columns =
                    [ { header = Element.text "Name"
                      , width = Element.fill
                      , view = .name >> Element.text
                      }
                    , { header = Element.text "Commencement"
                      , width = Element.fill
                      , view = .dateOfRegistration >> datestringEncoder >> Element.text
                      }
                    , { header = Element.text "Details"
                      , width = Element.fill
                      , view =
                            \tutor ->
                                Input.button
                                    []
                                    { label = Element.text "More"
                                    , onPress = Just (ToDetails tutor.id)
                                    }
                      }
                    ]
                , data = tutorList
                }


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewFilters model.filters
        , viewPagination model.pagination
        , viewData model.data
        , viewPagination model.pagination
        ]
