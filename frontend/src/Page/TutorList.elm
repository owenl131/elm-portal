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
import Colors
import Component.Paged as Paged
import Date
import DatePicker
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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


type alias TutorFiltersForm =
    { nameFilter : String
    , schoolFilter : String
    , dobLowerPicker : DatePicker.Model
    , dobUpperPicker : DatePicker.Model
    , joinLowerPicker : DatePicker.Model
    , joinUpperPicker : DatePicker.Model
    , dobLowerText : String
    , dobUpperText : String
    , joinLowerText : String
    , joinUpperText : String
    , classFilter : String
    }


initFromFilters : TutorFilters -> TutorFiltersForm
initFromFilters filters =
    { nameFilter = ""
    , schoolFilter = ""
    , dobLowerPicker = DatePicker.init
    , dobUpperPicker = DatePicker.init
    , joinLowerPicker = DatePicker.init
    , joinUpperPicker = DatePicker.init
    , dobLowerText = filters.dobLower |> Maybe.map Date.toIsoString |> Maybe.withDefault "No bound"
    , dobUpperText = filters.dobUpper |> Maybe.map Date.toIsoString |> Maybe.withDefault "No bound"
    , joinLowerText = filters.joinDateLower |> Maybe.map Date.toIsoString |> Maybe.withDefault "No bound"
    , joinUpperText = filters.joinDateUpper |> Maybe.map Date.toIsoString |> Maybe.withDefault "No bound"
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
    , data : WebData (Paged.Paged (List Tutor))
    }


type WhichDatePicker
    = JoinLower
    | JoinUpper
    | DobLower
    | DobUpper


type Msg
    = PaginationChanged Paged.Msg
    | ChangePicker WhichDatePicker DatePicker.ChangeEvent
    | SetToday Date.Date
    | ToDetails String
    | GotTutorList (Result Http.Error (Paged.Paged (List Tutor)))
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


fetchTutorList : TutorFilters -> Int -> Cmd Msg
fetchTutorList filters page =
    Http.get
        { url = "http://localhost:5000/tutors" ++ Builder.toQuery (Builder.int "page" page :: tutorFiltersToQueryList filters)
        , expect = Http.expectJson GotTutorList <| Paged.pagedDecoder (Decode.list tutorDecoder)
        }


init : Navigation.Key -> TutorFilters -> Int -> ( Model, Cmd Msg )
init key filters page =
    ( { key = key
      , filters = filters
      , filtersForm = initFromFilters filters
      , page = page
      , data = RemoteData.Loading
      }
    , Cmd.batch
        [ Task.perform SetToday Date.today
        , fetchTutorList filters page
        ]
    )


toggleStatuses : Tutor.TutorStatus -> TutorFilters -> TutorFilters
toggleStatuses status filters =
    { filters
        | statuses =
            if List.member status filters.statuses then
                List.filter ((/=) status) filters.statuses

            else
                status :: filters.statuses
    }


toggleAdminLvls : Tutor.AdminLevel -> TutorFilters -> TutorFilters
toggleAdminLvls adminLvl filters =
    { filters
        | admins =
            if List.member adminLvl filters.admins then
                List.filter ((/=) adminLvl) filters.admins

            else
                adminLvl :: filters.admins
    }


toggleGender : Tutor.Gender -> TutorFilters -> TutorFilters
toggleGender gender filters =
    { filters
        | genders =
            if List.member gender filters.genders then
                List.filter ((/=) gender) filters.genders

            else
                gender :: filters.genders
    }


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


updateText : WhichDatePicker -> String -> TutorFiltersForm -> TutorFiltersForm
updateText which text form =
    case which of
        JoinLower ->
            { form | joinLowerText = text }

        JoinUpper ->
            { form | joinUpperText = text }

        DobLower ->
            { form | dobLowerText = text }

        DobUpper ->
            { form | dobUpperText = text }


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
        PaginationChanged change ->
            case change of
                Paged.ChangePagePrevious ->
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

                Paged.ChangePageNext ->
                    let
                        newModel =
                            { model | page = model.page + 1 }
                    in
                    ( newModel, pushUrl newModel )

                Paged.ChangePage page ->
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
            let
                newModel =
                    { model | filters = { filters | names = filtersForm.nameFilter :: filters.names |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | nameFilter = "" } }
            in
            ( newModel, pushUrl newModel )

        AddSchoolFilter ->
            let
                newModel =
                    { model | filters = { filters | schools = filtersForm.schoolFilter :: filters.schools |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | schoolFilter = "" } }
            in
            ( newModel, pushUrl newModel )

        AddClassFilter ->
            let
                newModel =
                    { model | filters = { filters | classes = filtersForm.classFilter :: filters.classes |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | classFilter = "" } }
            in
            ( newModel, pushUrl newModel )

        RemoveNameFilter name ->
            let
                newModel =
                    { model | filters = { filters | names = List.filter ((/=) name) filters.names } }
            in
            ( newModel, pushUrl newModel )

        RemoveSchoolFilter school ->
            let
                newModel =
                    { model | filters = { filters | schools = List.filter ((/=) school) filters.schools } }
            in
            ( newModel, pushUrl newModel )

        RemoveClassFilter class ->
            let
                newModel =
                    { model | filters = { filters | classes = List.filter ((/=) class) filters.classes } }
            in
            ( newModel, pushUrl newModel )

        ToggleStatus status ->
            let
                newModel =
                    { model | filters = filters |> toggleStatuses status }
            in
            ( newModel, pushUrl newModel )

        ToggleAdminLvl adminLvl ->
            let
                newModel =
                    { model | filters = filters |> toggleAdminLvls adminLvl }
            in
            ( newModel, pushUrl newModel )

        ToggleGender gender ->
            let
                newModel =
                    { model | filters = filters |> toggleGender gender }
            in
            ( newModel, pushUrl newModel )

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
                    let
                        newModel =
                            { model
                                | filters = filters |> updateDate whichDatePicker (Just date)
                                , filtersForm = filtersForm |> updateText whichDatePicker (Date.toIsoString date)
                            }
                    in
                    ( newModel, pushUrl newModel )

                DatePicker.TextChanged text ->
                    case Date.fromIsoString text |> Result.toMaybe of
                        Nothing ->
                            ( { model | filtersForm = filtersForm |> updateText whichDatePicker text }, Cmd.none )

                        Just date ->
                            if Date.toIsoString date == text then
                                let
                                    newModel =
                                        { model
                                            | filters = filters |> updateDate whichDatePicker (Just date)
                                            , filtersForm = filtersForm |> updateText whichDatePicker text
                                        }
                                in
                                ( newModel
                                , pushUrl newModel
                                )

                            else
                                ( { model | filtersForm = filtersForm |> updateText whichDatePicker text }, Cmd.none )

                DatePicker.PickerChanged subMsg ->
                    ( { model | filtersForm = updatePicker whichDatePicker (DatePicker.update subMsg) filtersForm }
                    , Cmd.none
                    )


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
            Colors.grey

        activeGreen =
            Element.rgb 0 255 0

        inactiveWhite =
            Element.rgb 255 255 255

        backgroundColor : Bool -> Bool -> Element.Color
        backgroundColor disabled active =
            if disabled then
                Colors.clear

            else if active then
                activeGreen

            else
                inactiveWhite

        fontColor : Bool -> Bool -> Element.Color
        fontColor disabled active =
            if disabled then
                disabledGrey

            else if active then
                Colors.black

            else
                Colors.black
    in
    Element.row
        [ Element.spacing 5 ]
        (List.map
            (\( x, label ) ->
                Input.button
                    [ Background.color (backgroundColor filterUnused (List.member x selected))
                    , Font.color (fontColor filterUnused (List.member x selected))
                    , Element.paddingXY 5 2
                    , Border.rounded 3
                    , Border.width 1
                    ]
                    { label = Element.text label, onPress = Just (toggle x) }
            )
            all
        )


viewFilterSingle : (String -> Msg) -> String -> Element Msg
viewFilterSingle action label =
    Element.row
        [ Border.width 1
        , Border.rounded 3
        , Element.spacing 5
        , Element.paddingXY 5 2
        ]
        [ Element.text label
        , Input.button [] { label = Element.text "x", onPress = Just (action label) }
        ]


viewFilters : TutorFiltersForm -> TutorFilters -> Element Msg
viewFilters form filters =
    let
        textFieldStyles =
            [ Element.padding 4
            , Element.width <| Element.px 200
            ]

        dateFieldStyles =
            [ Element.padding 4
            , Element.width <| Element.px 100
            ]

        textLabelStyles =
            [ Element.width <| Element.px 100 ]
    in
    Element.row [ Element.width Element.fill, Background.color Colors.theme.p50 ]
        [ Element.column
            [ Element.padding 20
            , Element.spacing 8
            , Element.alignTop
            ]
            [ Element.row
                [ Element.spacing 4 ]
                [ DatePicker.input
                    dateFieldStyles
                    { onChange = ChangePicker JoinLower
                    , selected = filters.joinDateLower
                    , label = Input.labelHidden "Joined after"
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = form.joinLowerText
                    , model = form.joinLowerPicker
                    }
                , Element.row
                    [ Element.width <| Element.px 100
                    , Element.paddingXY 5 0
                    ]
                    [ Element.el
                        [ Element.alignLeft ]
                        (Element.text "<")
                    , Element.el
                        [ Element.centerX ]
                        (Element.text "Join Date")
                    , Element.el
                        [ Element.alignRight ]
                        (Element.text "<")
                    ]
                , DatePicker.input
                    dateFieldStyles
                    { onChange = ChangePicker JoinUpper
                    , selected = filters.joinDateUpper
                    , label = Input.labelHidden "Joined before"
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = form.joinUpperText
                    , model = form.joinUpperPicker
                    }
                ]
            , Element.row
                [ Element.spacing 4 ]
                [ DatePicker.input
                    dateFieldStyles
                    { onChange = ChangePicker DobLower
                    , selected = filters.dobLower
                    , label = Input.labelHidden "DOB after"
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = form.dobLowerText
                    , model = form.dobLowerPicker
                    }
                , Element.row
                    [ Element.width <| Element.px 100
                    , Element.paddingXY 5 0
                    ]
                    [ Element.el
                        [ Element.alignLeft ]
                        (Element.text "<")
                    , Element.el
                        [ Element.centerX ]
                        (Element.text "DOB")
                    , Element.el
                        [ Element.alignRight ]
                        (Element.text "<")
                    ]
                , DatePicker.input
                    dateFieldStyles
                    { onChange = ChangePicker DobUpper
                    , selected = filters.dobUpper
                    , label = Input.labelHidden "DOB before"
                    , placeholder = Nothing
                    , settings = DatePicker.defaultSettings
                    , text = form.dobUpperText
                    , model = form.dobUpperPicker
                    }
                ]
            , Element.row [ Element.spacing 4 ]
                [ Element.paragraph textLabelStyles [ Element.text "Filter Status" ]
                , viewToggleFilter [ ( Tutor.Active, "Active" ), ( Tutor.Inactive, "Inactive" ), ( Tutor.New, "New" ) ] ToggleStatus filters.statuses
                ]
            , Element.row [ Element.spacing 4 ]
                [ Element.paragraph textLabelStyles [ Element.text "Filter Gender" ]
                , viewToggleFilter [ ( Tutor.Male, "M" ), ( Tutor.Female, "F" ) ] ToggleGender filters.genders
                ]
            , Element.row [ Element.spacing 4 ]
                [ Element.paragraph textLabelStyles [ Element.text "Filter Role" ]
                , viewToggleFilter [ ( Tutor.LvlAdmin, "Admin" ), ( Tutor.LvlTutor, "Tutor" ) ] ToggleAdminLvl filters.admins
                ]
            , Element.el [ Element.width <| Element.px 10 ] Element.none
            ]
        , Element.column
            [ Element.padding 20
            , Element.spacing 8
            , Element.alignTop
            ]
            [ Element.row
                [ Element.spacing 4 ]
                ([ Input.text
                    textFieldStyles
                    { label = Input.labelLeft textLabelStyles (Element.text "Filter Name")
                    , onChange = EnteredNameFilter
                    , placeholder = Nothing
                    , text = form.nameFilter
                    }
                 , Input.button [] { label = Element.text "+", onPress = Just AddNameFilter }
                 ]
                    ++ List.map (viewFilterSingle RemoveNameFilter) filters.names
                )
            , Element.row
                [ Element.spacing 4 ]
                ([ Input.text
                    textFieldStyles
                    { label = Input.labelLeft textLabelStyles (Element.text "Filter School")
                    , onChange = EnteredSchoolFilter
                    , placeholder = Nothing
                    , text = form.schoolFilter
                    }
                 , Input.button [] { label = Element.text "+", onPress = Just AddSchoolFilter }
                 ]
                    ++ List.map (viewFilterSingle RemoveSchoolFilter) filters.schools
                )
            , Element.row
                [ Element.spacing 4 ]
                ([ Input.text
                    textFieldStyles
                    { label = Input.labelLeft textLabelStyles (Element.text "Filter Classes")
                    , onChange = EnteredClassFilter
                    , placeholder = Nothing
                    , text = form.classFilter
                    }
                 , Input.button [] { label = Element.text "+", onPress = Just AddClassFilter }
                 ]
                    ++ List.map (viewFilterSingle RemoveClassFilter) filters.classes
                )
            ]
        ]


viewData : WebData (Paged.Paged (List Tutor)) -> Element Msg
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
                [ Element.padding 20, Element.spacing 10 ]
                { columns =
                    [ { header = Element.text "Name" |> Element.el [ Font.bold ]
                      , width = Element.fill
                      , view = .name >> Element.text
                      }
                    , { header = Element.text "Email" |> Element.el [ Font.bold ]
                      , width = Element.fill
                      , view = .email >> Element.text
                      }
                    , { header = Element.text "Commencement" |> Element.el [ Font.bold ]
                      , width = Element.fill
                      , view = .dateOfRegistration >> Date.toIsoString >> Element.text
                      }
                    , { header = Element.text "Details" |> Element.el [ Font.bold ]
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


blankIfAbsent : (a -> Element msg) -> WebData a -> Element msg
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
        , Element.spacing 10
        ]
        [ viewFilters model.filtersForm model.filters
        , blankIfAbsent Paged.viewPagination model.data
            |> Element.map PaginationChanged
        , viewData model.data
        , blankIfAbsent Paged.viewPagination model.data
            |> Element.map PaginationChanged
        ]
