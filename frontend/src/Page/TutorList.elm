module Page.TutorList exposing
    ( Model
    , Msg
    , TutorFilters
    , init
    , tutorFiltersFromUrl
    , update
    , view
    )

import Browser.Navigation as Navigation exposing (pushUrl)
import Date
import DatePicker
import Element exposing (Element)
import Element.Background as Background
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import List.Extra
import Maybe.Extra
import RemoteData exposing (WebData)
import Task
import Tutor
    exposing
        ( Tutor
        , toGender
        , toTutorAdminLevel
        , toTutorStatus
        , tutorDecoder
        )
import Url.Builder as Builder
import Url.Parser.Query as Query


type alias Paged a =
    { page : Int
    , perPage : Int
    , lastPage : Int
    , total : Int
    , data : a
    }


pagedDecoder : Decode.Decoder a -> Decode.Decoder (Paged a)
pagedDecoder subDecoder =
    Decode.succeed Paged
        |> Pipeline.required "page" Decode.int
        |> Pipeline.required "perPage" Decode.int
        |> Pipeline.required "lastPage" Decode.int
        |> Pipeline.required "total" Decode.int
        |> Pipeline.required "data" subDecoder


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
    { statuses : List Tutor.TutorStatus
    , genders : List Tutor.Gender
    , admins : List Tutor.AdminLevel
    , names : List String
    , schools : List String
    , dobLower : Maybe Date.Date
    , dobUpper : Maybe Date.Date
    , joinDateLower : Maybe Date.Date
    , joinDateUpper : Maybe Date.Date
    , classes : List String
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


tutorFiltersToQueryList : TutorFilters -> List Builder.QueryParameter
tutorFiltersToQueryList filters =
    List.map (Tutor.genderToString >> Builder.string "gender") filters.genders
        ++ List.map (Tutor.tutorAdminLevelEncoder >> Builder.int "admin") filters.admins
        ++ List.map (Tutor.tutorStatusEncoder >> Builder.int "status") filters.statuses
        ++ List.map (Builder.string "name") filters.names
        ++ List.map (Builder.string "school") filters.schools
        ++ List.map (Date.toIsoString >> Builder.string "dobLower") (Maybe.Extra.toList filters.dobLower)
        ++ List.map (Date.toIsoString >> Builder.string "dobUpper") (Maybe.Extra.toList filters.dobUpper)
        ++ List.map (Date.toIsoString >> Builder.string "joinLower") (Maybe.Extra.toList filters.joinDateLower)
        ++ List.map (Date.toIsoString >> Builder.string "joinUpper") (Maybe.Extra.toList filters.joinDateUpper)
        ++ List.map (Builder.string "classes") filters.classes


type alias Model =
    { key : Navigation.Key
    , filters : TutorFilters
    , filtersForm : TutorFiltersForm
    , page : Int
    , data : WebData (Paged (List Tutor))
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
    | SetToday Date.Date
    | ToDetails String
    | GotTutorList (Result Http.Error (Paged (List Tutor)))
    | EnteredNameFilter String
    | EnteredSchoolFilter String
    | EnteredClassFilter String
    | AddNameFilter
    | AddSchoolFilter
    | AddClassFilter
    | RemoveNameFilter String
    | RemoveSchoolFilter String
    | RemoveClassFilter String
    | ToggleStatus Tutor.TutorStatus
    | ToggleGender Tutor.Gender
    | ToggleAdminLvl Tutor.AdminLevel
    | UpdateUrl


fetchTutorList : TutorFilters -> Int -> Cmd Msg
fetchTutorList filters page =
    Http.get
        { url = "http://localhost:5000/tutors" ++ Builder.toQuery (Builder.int "page" page :: tutorFiltersToQueryList filters)
        , expect = Http.expectJson GotTutorList <| pagedDecoder (Decode.list tutorDecoder)
        }


init : Navigation.Key -> TutorFilters -> Int -> ( Model, Cmd Msg )
init key filters page =
    ( { key = key
      , filters = filters
      , filtersForm = emptyForm
      , page = page
      , data = RemoteData.Loading
      }
    , Cmd.batch
        [ Task.perform SetToday Date.today
        , fetchTutorList filters page
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
            let
                newModel =
                    { model
                        | page =
                            if model.page == 0 then
                                model.page

                            else
                                model.page - 1
                    }
            in
            ( newModel, pushUrl newModel )

        ChangePageNext ->
            let
                newModel =
                    { model | page = model.page + 1 }
            in
            ( newModel, pushUrl newModel )

        ChangePage page ->
            let
                newModel =
                    { model | page = page }
            in
            ( newModel, pushUrl newModel )

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

        AddNameFilter ->
            ( { model | filters = { filters | names = filtersForm.nameFilter :: filters.names |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | nameFilter = "" } }, Cmd.none )

        AddSchoolFilter ->
            ( { model | filters = { filters | schools = filtersForm.schoolFilter :: filters.schools |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | schoolFilter = "" } }, Cmd.none )

        AddClassFilter ->
            ( { model | filters = { filters | classes = filtersForm.classFilter :: filters.classes |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | classFilter = "" } }, Cmd.none )

        RemoveNameFilter name ->
            ( { model | filters = { filters | names = List.filter ((/=) name) filters.names } }, Cmd.none )

        RemoveSchoolFilter school ->
            ( { model | filters = { filters | schools = List.filter ((/=) school) filters.schools } }, Cmd.none )

        RemoveClassFilter class ->
            ( { model | filters = { filters | classes = List.filter ((/=) class) filters.classes } }, Cmd.none )

        ToggleStatus status ->
            ( { model
                | filters =
                    { filters
                        | statuses =
                            if List.member status filters.statuses then
                                List.filter ((/=) status) filters.statuses

                            else
                                status :: filters.statuses
                    }
              }
            , Cmd.none
            )

        ToggleAdminLvl adminLvl ->
            ( { model
                | filters =
                    { filters
                        | admins =
                            if List.member adminLvl filters.admins then
                                List.filter ((/=) adminLvl) filters.admins

                            else
                                adminLvl :: filters.admins
                    }
              }
            , Cmd.none
            )

        ToggleGender gender ->
            ( { model
                | filters =
                    { filters
                        | genders =
                            if List.member gender filters.genders then
                                List.filter ((/=) gender) filters.genders

                            else
                                gender :: filters.genders
                    }
              }
            , Cmd.none
            )

        SetToday today ->
            ( { model
                | filtersForm =
                    filtersForm
                        |> updatePicker JoinUpper (DatePicker.setToday today)
                        |> updatePicker JoinLower (DatePicker.setToday today)
                        |> updatePicker DobUpper (DatePicker.setToday today)
                        |> updatePicker DobLower (DatePicker.setToday today)
              }
            , Cmd.none
            )

        ChangePicker whichDatePicker changeEvent ->
            case changeEvent of
                DatePicker.DateChanged date ->
                    ( { model
                        | filters =
                            updateDate whichDatePicker (Just date) filters
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

        UpdateUrl ->
            ( model, pushUrl model )


pushUrl : Model -> Cmd Msg
pushUrl model =
    Navigation.pushUrl
        model.key
        ("/tutors" ++ Builder.toQuery (Builder.int "page" model.page :: tutorFiltersToQueryList model.filters))


viewToggleFilter : List ( a, String ) -> (a -> Msg) -> List a -> Element Msg
viewToggleFilter all toggle selected =
    let
        filterUnused =
            List.isEmpty selected

        disabledGrey =
            Element.rgb255 200 200 200

        activeGreen =
            Element.rgb 0 255 0

        inactiveWhite =
            Element.rgb 255 255 255

        backgroundColor : Bool -> Bool -> Element.Color
        backgroundColor disabled active =
            if disabled then
                disabledGrey

            else if active then
                activeGreen

            else
                inactiveWhite
    in
    Element.row
        []
        (List.map
            (\( x, label ) ->
                Input.button
                    [ Background.color (backgroundColor filterUnused (List.member x selected)) ]
                    { label = Element.text label, onPress = Just (toggle x) }
            )
            all
        )


viewFilterSingle : (String -> Msg) -> String -> Element Msg
viewFilterSingle action label =
    Element.row
        [ Background.color <| Element.rgb255 100 100 255 ]
        [ Element.text label
        , Input.button [] { label = Element.text "x", onPress = Just (action label) }
        ]


viewFilters : TutorFiltersForm -> TutorFilters -> Element Msg
viewFilters form filters =
    Element.row
        [ Element.width Element.fill
        ]
        [ Element.column
            [ Element.width Element.fill ]
            [ Element.row
                []
                ([ Input.text
                    []
                    { label = Input.labelLeft [] (Element.text "Filter Name")
                    , onChange = EnteredNameFilter
                    , placeholder = Nothing
                    , text = form.nameFilter
                    }
                 , Input.button [] { label = Element.text "+", onPress = Just AddNameFilter }
                 ]
                    ++ List.map (viewFilterSingle RemoveNameFilter) filters.names
                )
            , Element.row
                []
                ([ Input.text
                    []
                    { label = Input.labelLeft [] (Element.text "Filter School")
                    , onChange = EnteredSchoolFilter
                    , placeholder = Nothing
                    , text = form.schoolFilter
                    }
                 , Input.button [] { label = Element.text "+", onPress = Just AddSchoolFilter }
                 ]
                    ++ List.map (viewFilterSingle RemoveSchoolFilter) filters.schools
                )
            , Element.row
                []
                ([ Input.text
                    []
                    { label = Input.labelLeft [] (Element.text "Filter Classes")
                    , onChange = EnteredClassFilter
                    , placeholder = Nothing
                    , text = form.classFilter
                    }
                 , Input.button [] { label = Element.text "+", onPress = Just AddClassFilter }
                 ]
                    ++ List.map (viewFilterSingle RemoveClassFilter) filters.classes
                )
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
            , Element.row []
                [ Element.text "Filter by Status: "
                , viewToggleFilter [ ( Tutor.Active, "Active" ), ( Tutor.Inactive, "Inactive" ), ( Tutor.New, "New" ) ] ToggleStatus filters.statuses
                ]
            , Element.row []
                [ Element.text "Filter by Gender: "
                , viewToggleFilter [ ( Tutor.Male, "M" ), ( Tutor.Female, "F" ) ] ToggleGender filters.genders
                ]
            , Element.row []
                [ Element.text "Filter by Role: "
                , viewToggleFilter [ ( Tutor.LvlAdmin, "Admin" ), ( Tutor.LvlTutor, "Tutor" ) ] ToggleAdminLvl filters.admins
                ]
            , Input.button [] { label = Element.text "Update", onPress = Just UpdateUrl }
            ]
        ]


viewPagination : Paged a -> Element Msg
viewPagination pagedData =
    Element.row
        [ Element.width Element.fill ]
        (Input.button [] { onPress = Just ChangePagePrevious, label = Element.text "<" }
            :: List.map
                (\p -> Input.button [] { onPress = Just (ChangePage (p - 1)), label = Element.text (String.fromInt p) })
                (List.range 1 pagedData.lastPage)
            ++ [ Input.button [] { onPress = Just ChangePageNext, label = Element.text ">" } ]
        )


viewData : WebData (Paged (List Tutor)) -> Element Msg
viewData data =
    case data of
        RemoteData.NotAsked ->
            Element.text "Not asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure err ->
            Element.text (Debug.toString err)

        RemoteData.Success pagedData ->
            let
                tutorList =
                    pagedData.data
            in
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
                      , view = .dateOfRegistration >> Date.toIsoString >> Element.text
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


blankIfAbsent : (a -> Element Msg) -> WebData a -> Element Msg
blankIfAbsent viewIt webData =
    case webData of
        RemoteData.Success data ->
            viewIt data

        _ ->
            Element.none


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewFilters model.filtersForm model.filters
        , blankIfAbsent viewPagination model.data
        , viewData model.data
        , blankIfAbsent viewPagination model.data
        ]
