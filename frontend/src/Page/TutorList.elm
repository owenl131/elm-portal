module Page.TutorList exposing
    ( Model
    , Msg
    , TutorFilters
    , init
    , tutorFiltersFromUrl
    , update
    , view
    )

import Api
import Base64
import Browser.Navigation as Navigation exposing (pushUrl)
import Colors
import Component.Paged as Paged
import Date
import DatePicker
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import List.Extra
import Maybe.Extra
import RemoteData exposing (WebData)
import Styles
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


updatePickerWithDate : Maybe Date.Date -> DatePicker.Model -> DatePicker.Model
updatePickerWithDate maybeDate =
    maybeDate |> Maybe.map DatePicker.setVisibleMonth |> Maybe.withDefault Basics.identity


initFromFilters : TutorFilters -> TutorFiltersForm
initFromFilters filters =
    { nameFilter = ""
    , schoolFilter = ""
    , dobLowerPicker = DatePicker.init |> updatePickerWithDate filters.dobLower
    , dobUpperPicker = DatePicker.init |> updatePickerWithDate filters.dobUpper
    , joinLowerPicker = DatePicker.init |> updatePickerWithDate filters.joinDateLower
    , joinUpperPicker = DatePicker.init |> updatePickerWithDate filters.joinDateUpper
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
    , credentials : Api.Credentials
    , filters : TutorFilters
    , filtersForm : TutorFiltersForm
    , page : Int
    , hoveredIndex : Int
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
    | DateCleared WhichDatePicker
    | SetToday Date.Date
    | ToDetails String
    | ToNew
    | TableHover Int
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


fetchTutorList : Api.Credentials -> TutorFilters -> Int -> Cmd Msg
fetchTutorList credentials filters page =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutors" ] (Builder.int "page" page :: tutorFiltersToQueryList filters)
        , expect = Http.expectJson GotTutorList <| Paged.pagedDecoder (Decode.list tutorDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


init : Api.Credentials -> Navigation.Key -> TutorFilters -> Int -> ( Model, Cmd Msg )
init credentials key filters page =
    ( { key = key
      , credentials = credentials
      , filters = filters
      , filtersForm = initFromFilters filters
      , page = page
      , data = RemoteData.Loading
      , hoveredIndex = -1
      }
    , Cmd.batch
        [ Task.perform SetToday Date.today
        , fetchTutorList credentials filters page
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

        ignore =
            ( model, Cmd.none )
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

        ToNew ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "tutors", "new" ] []) )

        EnteredNameFilter name ->
            ( { model | filtersForm = { filtersForm | nameFilter = name } }, Cmd.none )

        EnteredSchoolFilter school ->
            ( { model | filtersForm = { filtersForm | schoolFilter = school } }, Cmd.none )

        EnteredClassFilter class ->
            ( { model | filtersForm = { filtersForm | classFilter = class } }, Cmd.none )

        AddNameFilter ->
            if filtersForm.nameFilter == "" then
                ignore

            else
                let
                    newModel =
                        { model | filters = { filters | names = filtersForm.nameFilter :: filters.names |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | nameFilter = "" } }
                in
                ( newModel, pushUrl newModel )

        AddSchoolFilter ->
            if filtersForm.schoolFilter == "" then
                ignore

            else
                let
                    newModel =
                        { model | filters = { filters | schools = filtersForm.schoolFilter :: filters.schools |> List.sort |> List.Extra.unique }, filtersForm = { filtersForm | schoolFilter = "" } }
                in
                ( newModel, pushUrl newModel )

        AddClassFilter ->
            if filtersForm.classFilter == "" then
                ignore

            else
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

        TableHover index ->
            ( { model | hoveredIndex = index }, Cmd.none )

        SetToday today ->
            ( { model
                | filtersForm =
                    filtersForm
                        |> updatePicker JoinUpper (DatePicker.setToday today)
                        |> updatePicker JoinUpper (updatePickerWithDate filters.joinDateUpper)
                        |> updatePicker JoinLower (DatePicker.setToday today)
                        |> updatePicker JoinLower (updatePickerWithDate filters.joinDateLower)
                        |> updatePicker DobUpper (DatePicker.setToday today)
                        |> updatePicker DobUpper (updatePickerWithDate filters.dobUpper)
                        |> updatePicker DobLower (DatePicker.setToday today)
                        |> updatePicker DobLower (updatePickerWithDate filters.dobLower)
              }
            , Cmd.none
            )

        DateCleared whichDatePicker ->
            let
                newModel =
                    { model
                        | filters = filters |> updateDate whichDatePicker Nothing
                        , filtersForm =
                            filtersForm
                                |> updateText whichDatePicker "Not bound"
                    }
            in
            ( newModel, pushUrl newModel )

        ChangePicker whichDatePicker changeEvent ->
            case changeEvent of
                DatePicker.DateChanged date ->
                    let
                        newModel =
                            { model
                                | filters = filters |> updateDate whichDatePicker (Just date)
                                , filtersForm =
                                    filtersForm
                                        |> updateText whichDatePicker (Date.toIsoString date)
                                        |> updatePicker whichDatePicker (DatePicker.setVisibleMonth date)
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
                                            , filtersForm =
                                                filtersForm
                                                    |> updateText whichDatePicker text
                                                    |> updatePicker whichDatePicker (DatePicker.setVisibleMonth date)
                                        }
                                in
                                ( newModel
                                , pushUrl newModel
                                )

                            else
                                ( { model | filtersForm = filtersForm |> updateText whichDatePicker text }, Cmd.none )

                DatePicker.PickerChanged subMsg ->
                    ( { model
                        | filtersForm = updatePicker whichDatePicker (DatePicker.update subMsg) filtersForm
                      }
                    , Cmd.none
                    )


pushUrl : Model -> Cmd Msg
pushUrl model =
    Navigation.pushUrl
        model.key
        (Builder.absolute
            [ "tutors" ]
            (Builder.int "page" model.page :: tutorFiltersToQueryList model.filters)
        )


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
            Colors.white

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
        , Element.spacing 8
        ]
        [ Element.text label |> Element.el [ Element.centerY, Element.paddingXY 5 3 ]
        , Input.button
            [ Element.height Element.fill
            , Element.mouseOver [ Background.color Colors.theme.a100 ]
            , Element.paddingXY 3 3
            ]
            { label = Element.text "x", onPress = Just (action label) }
        ]


viewFilters : TutorFiltersForm -> TutorFilters -> Element Msg
viewFilters form filters =
    let
        crossButtonStyle =
            [ Border.roundEach { topLeft = 0, bottomLeft = 0, bottomRight = 3, topRight = 3 }
            , Element.paddingXY 5 4
            , Background.color Colors.theme.p50
            , Element.mouseOver [ Background.color Colors.theme.a100 ]
            ]
    in
    Element.row [ Element.width Element.fill, Background.color Colors.theme.p50 ]
        [ Element.column
            [ Element.padding 20
            , Element.spacing 8
            , Element.alignTop
            ]
            [ Element.row
                [ Element.spacing 5 ]
                [ DatePicker.input
                    (Element.inFront (Input.button (crossButtonStyle ++ [ Element.centerY, Element.alignRight ]) { onPress = Just (DateCleared JoinLower), label = Element.text "x" })
                        :: Styles.dateFieldStyle
                    )
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
                    (Element.inFront (Input.button (crossButtonStyle ++ [ Element.centerY, Element.alignRight ]) { onPress = Just (DateCleared JoinUpper), label = Element.text "x" })
                        :: Styles.dateFieldStyle
                    )
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
                [ Element.spacing 5 ]
                [ DatePicker.input
                    (Element.inFront (Input.button (crossButtonStyle ++ [ Element.centerY, Element.alignRight ]) { onPress = Just (DateCleared DobLower), label = Element.text "x" })
                        :: Styles.dateFieldStyle
                    )
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
                    (Element.inFront (Input.button (crossButtonStyle ++ [ Element.centerY, Element.alignRight ]) { onPress = Just (DateCleared DobUpper), label = Element.text "x" })
                        :: Styles.dateFieldStyle
                    )
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
                [ Element.paragraph Styles.textLabelStyle [ Element.text "Filter Status" ]
                , viewToggleFilter [ ( Tutor.Active, "Active" ), ( Tutor.Inactive, "Inactive" ), ( Tutor.New, "New" ) ] ToggleStatus filters.statuses
                ]
            , Element.row [ Element.spacing 4 ]
                [ Element.paragraph Styles.textLabelStyle [ Element.text "Filter Gender" ]
                , viewToggleFilter [ ( Tutor.Male, "M" ), ( Tutor.Female, "F" ) ] ToggleGender filters.genders
                ]
            , Element.row [ Element.spacing 4 ]
                [ Element.paragraph Styles.textLabelStyle [ Element.text "Filter Role" ]
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
                    Styles.textFieldStyle
                    { label = Input.labelLeft Styles.textLabelStyle (Element.text "Filter Name")
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
                    Styles.textFieldStyle
                    { label = Input.labelLeft Styles.textLabelStyle (Element.text "Filter School")
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
                    Styles.textFieldStyle
                    { label = Input.labelLeft Styles.textLabelStyle (Element.text "Filter Classes")
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


viewData : Int -> WebData (Paged.Paged (List Tutor)) -> Element Msg
viewData hovered data =
    case data of
        RemoteData.NotAsked ->
            Element.text "Not asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure err ->
            Element.text (Api.errorToString err)

        RemoteData.Success pagedData ->
            let
                tutorList =
                    pagedData.data

                toHeader : String -> Element Msg
                toHeader text =
                    text
                        |> Element.text
                        |> Element.el
                            [ Font.bold
                            , Element.paddingEach { top = 0, bottom = 8, left = 5, right = 5 }
                            ]

                cell : (Tutor -> Element Msg) -> Int -> Tutor -> Element Msg
                cell toElem index e =
                    Element.el
                        ([ Element.centerY
                         , Element.Events.onMouseEnter (TableHover index)
                         , Element.Events.onMouseLeave (TableHover -1)
                         , Element.Events.onDoubleClick (ToDetails e.id)
                         , Element.height Element.fill
                         , Element.padding 4
                         ]
                            ++ (if index == hovered then
                                    [ Background.color Colors.theme.p100 ]

                                else
                                    []
                               )
                        )
                        (toElem e |> Element.el [ Element.centerY ])
            in
            Element.indexedTable
                [ Element.padding 20 ]
                { columns =
                    [ { header = "Name" |> toHeader
                      , width = Element.fill |> Element.maximum 200
                      , view = .name >> Element.text |> cell
                      }
                    , { header = "School" |> toHeader
                      , width = Element.fill |> Element.maximum 150
                      , view = .school >> Element.text |> cell
                      }
                    , { header = "Role" |> toHeader
                      , width = Element.fill |> Element.maximum 80
                      , view = .admin >> Tutor.adminLevelAsString >> Element.text |> cell
                      }
                    , { header = "Status" |> toHeader
                      , width = Element.fill |> Element.maximum 70
                      , view = .status >> Tutor.tutorStatusAsString >> Element.text |> cell
                      }
                    , { header = "Email" |> toHeader
                      , width = Element.fill |> Element.maximum 150
                      , view = .email >> Element.text |> cell
                      }
                    , { header = "Joined" |> toHeader
                      , width = Element.fill |> Element.maximum 100
                      , view = .dateOfRegistration >> Date.toIsoString >> Element.text |> cell
                      }
                    , { header = "Details" |> toHeader
                      , width = Element.fill |> Element.maximum 60
                      , view =
                            (\tutor ->
                                Input.button
                                    [ Background.color Colors.theme.a400
                                    , Border.width 1
                                    , Border.rounded 3
                                    , Element.paddingXY 10 2
                                    , Element.mouseOver [ Background.color Colors.theme.a200 ]
                                    ]
                                    { label = Element.text "More" |> Element.el [ Element.centerX ]
                                    , onPress = Just (ToDetails tutor.id)
                                    }
                            )
                                |> cell
                      }
                    ]
                , data = tutorList
                }


viewActionBar : Element Msg
viewActionBar =
    let
        buttonStyles =
            [ Element.width (Element.px 120 |> Element.minimum 80)
            , Background.color Colors.theme.p200
            , Element.mouseOver [ Background.color Colors.theme.a200 ]
            , Border.width 1
            , Border.rounded 3
            , Element.paddingXY 20 10
            ]
    in
    Element.row
        [ Element.padding 2
        , Element.spacing 5
        , Element.width Element.fill
        ]
        [ Input.button buttonStyles
            { onPress = Just ToNew
            , label = Element.text "Add New" |> Element.el [ Element.centerX, Element.centerY ]
            }
        , Input.button buttonStyles
            { onPress = Nothing
            , label = Element.text "Export" |> Element.el [ Element.centerX, Element.centerY ]
            }
        , Input.button buttonStyles
            { onPress = Nothing
            , label = Element.text "Import" |> Element.el [ Element.centerX, Element.centerY ]
            }
        , Input.button buttonStyles
            { onPress = Nothing
            , label = Element.text "Demographics" |> Element.el [ Element.centerX, Element.centerY ]
            }
        ]


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
        [ viewActionBar
        , viewFilters model.filtersForm model.filters
        , blankIfAbsent Paged.viewPagination model.data
            |> Element.map PaginationChanged
        , viewData model.hoveredIndex model.data
        , blankIfAbsent Paged.viewPagination model.data
            |> Element.map PaginationChanged
        ]
