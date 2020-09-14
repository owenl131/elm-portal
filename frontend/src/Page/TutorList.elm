module Page.TutorList exposing (Model, Msg, Pagination, TutorFilters, init, tutorFiltersFromUrl, update, view)

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
import Tutor
    exposing
        ( AdminLevel
        , Gender
        , Tutor
        , TutorStatus
        , datestringEncoder
        , toGender
        , toTutorAdminLevel
        , toTutorStatus
        , tutorDecoder
        )
import Url.Parser.Query as Query


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


type alias TutorFiltersForm =
    { nameFilter : String
    , schoolFilter : String
    , dobLowerPicker : DatePicker.Model
    , dobUpperPicker : DatePicker.Model
    , joinLowerPicker : DatePicker.Model
    , joinUpperPicker : DatePicker.Model
    , classFilter : String
    }


emptyForm : TutorFiltersForm
emptyForm =
    { nameFilter = ""
    , schoolFilter = ""
    , dobLowerPicker = DatePicker.init
    , dobUpperPicker = DatePicker.init
    , joinLowerPicker = DatePicker.init
    , joinUpperPicker = DatePicker.init
    , classFilter = ""
    }


type alias TutorFilters =
    { statuses : List TutorStatus
    , genders : List Gender
    , admins : List AdminLevel
    , names : List String
    , schools : List String
    , dobLower : Maybe Date.Date
    , dobUpper : Maybe Date.Date
    , joinDateLower : Maybe Date.Date
    , joinDateUpper : Maybe Date.Date
    , classes : List String
    }


emptyTutorFilter : TutorFilters
emptyTutorFilter =
    { statuses = []
    , genders = []
    , admins = []
    , names = []
    , schools = []
    , dobLower = Nothing
    , dobUpper = Nothing
    , joinDateLower = Nothing
    , joinDateUpper = Nothing
    , classes = []
    }


applyParser : Query.Parser a -> Query.Parser (a -> b) -> Query.Parser b
applyParser argParser funcParser =
    Query.map2 (<|) funcParser argParser


tutorFiltersFromUrl : Query.Parser TutorFilters
tutorFiltersFromUrl =
    -- Handle the two widgets
    Query.map TutorFilters
        (Query.custom "status" (List.filterMap (String.toInt >> Maybe.andThen toTutorStatus)))
        |> applyParser (Query.custom "gender" (List.filterMap toGender))
        |> applyParser (Query.custom "admin" (List.filterMap (String.toInt >> Maybe.andThen toTutorAdminLevel)))
        |> applyParser (Query.custom "name" (\x -> x))
        |> applyParser (Query.custom "school" (\x -> x))
        |> applyParser (Query.string "dobLower" |> Query.map (Maybe.andThen (Date.fromIsoString >> Result.toMaybe)))
        |> applyParser (Query.string "dobUpper" |> Query.map (Maybe.andThen (Date.fromIsoString >> Result.toMaybe)))
        |> applyParser (Query.string "joinLower" |> Query.map (Maybe.andThen (Date.fromIsoString >> Result.toMaybe)))
        |> applyParser (Query.string "joinUpper" |> Query.map (Maybe.andThen (Date.fromIsoString >> Result.toMaybe)))
        |> applyParser (Query.custom "classes" (\x -> x))


type alias Model =
    { key : Navigation.Key
    , pagination : Pagination
    , filters : TutorFilters
    , filtersForm : TutorFiltersForm
    , data : WebData (List Tutor)
    , joinLowerDate : Maybe Date.Date
    , joinUpperDate : Maybe Date.Date
    }


type WhichDatePicker
    = JoinLower
    | JoinUpper
    | DobLower
    | DobUpper


type Msg
    = ChangePagePrevious
    | ChangePageNext
    | ChangePage Int
    | ChangePicker WhichDatePicker DatePicker.ChangeEvent
    | SetToday WhichDatePicker Date.Date
    | ToDetails String
    | GotTutorList (Result Http.Error (List Tutor))
    | EnteredNameFilter String
    | EnteredSchoolFilter String
    | EnteredClassFilter String


init : Navigation.Key -> TutorFilters -> ( Model, Cmd Msg )
init key filters =
    ( { key = key
      , pagination = defaultPagination
      , filters = filters
      , filtersForm = emptyForm
      , data = RemoteData.Loading
      , joinLowerDate = Maybe.Nothing
      , joinUpperDate = Maybe.Nothing
      }
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:5000/tutors"
            , expect = Http.expectJson GotTutorList <| Decode.list tutorDecoder
            }
        , Cmd.batch
            [ Task.perform (SetToday JoinUpper) Date.today
            , Task.perform (SetToday JoinLower) Date.today
            , Task.perform (SetToday DobLower) Date.today
            , Task.perform (SetToday DobUpper) Date.today
            ]
        ]
    )


updateDate : WhichDatePicker -> Maybe Date.Date -> TutorFilters -> TutorFilters
updateDate which date filters =
    case which of
        JoinLower ->
            { filters | joinDateLower = date }

        JoinUpper ->
            { filters | joinDateUpper = date }

        DobLower ->
            { filters | dobLower = date }

        DobUpper ->
            { filters | dobUpper = date }


updatePicker : WhichDatePicker -> (DatePicker.Model -> DatePicker.Model) -> TutorFiltersForm -> TutorFiltersForm
updatePicker which modelUpdate filtersForm =
    case which of
        JoinLower ->
            { filtersForm | joinLowerPicker = filtersForm.joinLowerPicker |> modelUpdate }

        JoinUpper ->
            { filtersForm | joinUpperPicker = filtersForm.joinUpperPicker |> modelUpdate }

        DobLower ->
            { filtersForm | dobLowerPicker = filtersForm.dobLowerPicker |> modelUpdate }

        DobUpper ->
            { filtersForm | dobUpperPicker = filtersForm.dobUpperPicker |> modelUpdate }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        filters =
            model.filters

        filtersForm =
            model.filtersForm
    in
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

        EnteredNameFilter name ->
            ( { model | filtersForm = { filtersForm | nameFilter = name } }, Cmd.none )

        EnteredSchoolFilter school ->
            ( { model | filtersForm = { filtersForm | schoolFilter = school } }, Cmd.none )

        EnteredClassFilter class ->
            ( { model | filtersForm = { filtersForm | classFilter = class } }, Cmd.none )

        SetToday whichDatePicker today ->
            ( { model
                | filtersForm = updatePicker whichDatePicker (DatePicker.setToday today) filtersForm
              }
            , Cmd.none
            )

        ChangePicker whichDatePicker changeEvent ->
            case changeEvent of
                DatePicker.DateChanged date ->
                    ( { model
                        | filters = updateDate whichDatePicker (Just date) filters
                      }
                    , Cmd.none
                    )

                DatePicker.TextChanged text ->
                    ( { model
                        | filters =
                            updateDate whichDatePicker
                                (Date.fromIsoString text
                                    |> Result.toMaybe
                                    |> Maybe.Extra.orElse filters.joinDateLower
                                )
                                filters
                      }
                    , Cmd.none
                    )

                DatePicker.PickerChanged subMsg ->
                    ( { model | filtersForm = updatePicker whichDatePicker (DatePicker.update subMsg) filtersForm }
                    , Cmd.none
                    )


viewFilters : TutorFiltersForm -> TutorFilters -> Element Msg
viewFilters form filters =
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
                    , text = form.nameFilter
                    }
                , Input.button [] { label = Element.text "+", onPress = Nothing }
                ]
            , Element.row
                []
                [ Input.text
                    []
                    { label = Input.labelLeft [] (Element.text "Filter School")
                    , onChange = EnteredSchoolFilter
                    , placeholder = Nothing
                    , text = form.schoolFilter
                    }
                , Input.button [] { label = Element.text "+", onPress = Nothing }
                ]
            , Element.row
                []
                [ Input.text
                    []
                    { label = Input.labelLeft [] (Element.text "Filter Classes")
                    , onChange = EnteredClassFilter
                    , placeholder = Nothing
                    , text = form.classFilter
                    }
                , Input.button [] { label = Element.text "+", onPress = Nothing }
                ]
            , Element.row
                []
                [ DatePicker.input
                    []
                    { onChange = ChangePicker JoinLower
                    , selected = filters.joinDateLower
                    , label = Input.labelLeft [] (Element.text "Joined after")
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = Maybe.withDefault "No bound" (Maybe.map Date.toIsoString filters.joinDateLower)
                    , model = form.joinLowerPicker
                    }
                , DatePicker.input
                    []
                    { onChange = ChangePicker JoinUpper
                    , selected = filters.joinDateUpper
                    , label = Input.labelLeft [] (Element.text "Joined before")
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = Maybe.withDefault "No bound" (Maybe.map Date.toIsoString filters.joinDateUpper)
                    , model = form.joinUpperPicker
                    }
                ]
            , Element.row
                []
                [ DatePicker.input
                    []
                    { onChange = ChangePicker DobLower
                    , selected = filters.dobLower
                    , label = Input.labelLeft [] (Element.text "DOB after")
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = Maybe.withDefault "No bound" (Maybe.map Date.toIsoString filters.dobLower)
                    , model = form.dobLowerPicker
                    }
                , DatePicker.input
                    []
                    { onChange = ChangePicker DobUpper
                    , selected = filters.dobUpper
                    , label = Input.labelLeft [] (Element.text "DOB before")
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = Maybe.withDefault "No bound" (Maybe.map Date.toIsoString filters.dobUpper)
                    , model = form.dobUpperPicker
                    }
                ]
            ]
        , Element.column
            []
            [ Element.text "Filters:" ]
        ]


viewPagination : Pagination -> Element Msg
viewPagination _ =
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
                    , { header = Element.text "Email"
                      , width = Element.fill
                      , view = .email >> Element.text
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
        [ viewFilters model.filtersForm model.filters
        , viewPagination model.pagination
        , viewData model.data
        , viewPagination model.pagination
        ]
