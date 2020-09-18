module Page.ClassList exposing (ClassFilters, Model, Msg, classFiltersFromUrl, init, update, view)

import Browser.Navigation as Navigation
import Class exposing (Class, classDecoder)
import Colors
import Component.Paged as Paged
import Date
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Maybe.Extra
import RemoteData exposing (WebData)
import Time
import Url.Builder as Builder
import Url.Parser.Query as Query


type alias ClassFiltersForm =
    { nameFilter : String
    }


type alias ClassFilters =
    { names : List String
    , yearLower : Int
    , yearUpper : Int
    , days : List Date.Weekday
    }


classFiltersFromUrl : Query.Parser ClassFilters
classFiltersFromUrl =
    Query.map4 ClassFilters
        (Query.custom "name" (\x -> x))
        (Query.map (Maybe.withDefault 2020) (Query.int "yearLower"))
        (Query.map (Maybe.withDefault 2020) (Query.int "yearUpper"))
        (Query.custom "days" (List.filterMap (String.toInt >> Maybe.map Date.numberToWeekday)))


classFiltersToQueryList : ClassFilters -> List Builder.QueryParameter
classFiltersToQueryList filters =
    List.map (Builder.int "yearLower") [ filters.yearLower ]
        ++ List.map (Builder.int "yearUpper") [ filters.yearUpper ]
        ++ List.map (Builder.string "name") filters.names
        ++ List.map (Date.weekdayToNumber >> Builder.int "days") filters.days


type alias Model =
    { key : Navigation.Key
    , page : Int
    , filters : ClassFilters
    , filtersForm : ClassFiltersForm
    , data : WebData (Paged.Paged (List Class))
    }


type Msg
    = PaginationChanged Paged.Msg
    | GotClassList (Result Http.Error (Paged.Paged (List Class)))
    | ToDetails Int
    | EnteredNameFilter String
    | AddNameFilter
    | RemoveNameFilter String
    | EnteredYearLowerFilter Int
    | EnteredYearUpperFilter Int
    | ToggleDay Date.Weekday


init : Navigation.Key -> ClassFilters -> Int -> ( Model, Cmd Msg )
init key filters page =
    ( { key = key
      , page = page
      , filters = filters
      , filtersForm =
            { nameFilter = "" }
      , data = RemoteData.Loading
      }
    , Http.get
        { url = "http://localhost:5000/classes" ++ Builder.toQuery (classFiltersToQueryList filters)
        , expect = Http.expectJson GotClassList <| Paged.pagedDecoder (Decode.list classDecoder)
        }
    )


pushUrl : Model -> Cmd Msg
pushUrl model =
    Navigation.pushUrl
        model.key
        ("/classes" ++ Builder.toQuery (Builder.int "page" model.page :: classFiltersToQueryList model.filters))


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
                Paged.ChangePageNext ->
                    let
                        newModel =
                            { model | page = model.page + 1 }
                    in
                    ( newModel, pushUrl newModel )

                Paged.ChangePagePrevious ->
                    let
                        newModel =
                            { model | page = model.page - 1 }
                    in
                    ( newModel, pushUrl newModel )

                Paged.ChangePage page ->
                    let
                        newModel =
                            { model | page = page }
                    in
                    ( newModel, pushUrl newModel )

        GotClassList result ->
            ( { model | data = RemoteData.fromResult result }, Cmd.none )

        ToDetails id ->
            ( model, Navigation.pushUrl model.key ("/class/" ++ String.fromInt id) )

        EnteredNameFilter name ->
            ( { model | filtersForm = { filtersForm | nameFilter = name } }, Cmd.none )

        AddNameFilter ->
            let
                newModel =
                    { model | filters = { filters | names = filtersForm.nameFilter :: filters.names } }
            in
            ( newModel, pushUrl newModel )

        RemoveNameFilter name ->
            let
                newModel =
                    { model | filters = { filters | names = List.filter ((/=) name) filters.names } }
            in
            ( newModel, pushUrl newModel )

        EnteredYearLowerFilter year ->
            let
                newModel =
                    { model | filters = { filters | yearLower = year, yearUpper = Basics.max filters.yearUpper year } }
            in
            ( newModel, pushUrl newModel )

        EnteredYearUpperFilter year ->
            let
                newModel =
                    { model | filters = { filters | yearUpper = year, yearLower = Basics.min filters.yearLower year } }
            in
            ( newModel, pushUrl newModel )

        ToggleDay day ->
            let
                newModel =
                    if List.member day filters.days then
                        { model | filters = { filters | days = List.filter ((/=) day) filters.days } }

                    else
                        { model | filters = { filters | days = day :: filters.days } }
            in
            ( newModel, pushUrl newModel )


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


viewClassFilters : ClassFiltersForm -> ClassFilters -> Element Msg
viewClassFilters form filters =
    let
        textFieldStyles =
            [ Element.padding 4
            , Element.width <| Element.px 200
            ]

        textLabelStyles =
            [ Element.width <| Element.px 100 ]
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 10
        , Background.color Colors.theme.p50
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
        , Element.row [ Element.spacing 4 ]
            [ Element.paragraph textLabelStyles [ Element.text "Filter Status" ]
            , viewToggleFilter
                [ ( Time.Mon, "Mon" )
                , ( Time.Tue, "Tue" )
                , ( Time.Wed, "Wed" )
                , ( Time.Thu, "Thu" )
                , ( Time.Fri, "Fri" )
                , ( Time.Sat, "Sat" )
                , ( Time.Sun, "Sun" )
                ]
                ToggleDay
                filters.days
            ]
        , Input.slider
            [ Element.height (Element.px 10)
            , Element.width (Element.fill |> Element.maximum 300)
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color Colors.grey
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { label = Input.labelLeft [ Element.width (Element.px 170) ] (Element.text ("Year Lower Bound: " ++ String.fromInt filters.yearLower))
            , max = 2030
            , min = 2010
            , step = Just 1
            , onChange = Basics.round >> EnteredYearLowerFilter
            , thumb = Input.defaultThumb
            , value = Basics.toFloat filters.yearLower
            }
        , Input.slider
            [ Element.height (Element.px 10)
            , Element.width (Element.fill |> Element.maximum 300)
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color Colors.grey
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { label = Input.labelLeft [ Element.width (Element.px 170) ] (Element.text ("Year Upper Bound: " ++ String.fromInt filters.yearUpper))
            , max = 2030
            , min = 2010
            , step = Just 1
            , onChange = Basics.round >> EnteredYearUpperFilter
            , thumb = Input.defaultThumb
            , value = Basics.toFloat filters.yearUpper
            }
        ]


viewData : WebData (Paged.Paged (List Class)) -> Element Msg
viewData data =
    case data of
        RemoteData.NotAsked ->
            Element.text "Not asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure error ->
            Element.text (Debug.toString error)

        RemoteData.Success pagedData ->
            let
                classList =
                    pagedData.data

                toHeader : String -> Element Msg
                toHeader text =
                    text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, bottom = 5, left = 0, right = 3 } ]
            in
            Element.table
                [ Element.padding 20, Element.spacing 5 ]
                { columns =
                    [ { header = "Name" |> toHeader
                      , width = Element.fill |> Element.maximum 200
                      , view = .name >> Element.text >> Element.el [ Element.centerY ]
                      }
                    , { header = "Days" |> toHeader
                      , width = Element.fill |> Element.maximum 150
                      , view = .days >> List.map Debug.toString >> List.intersperse ", " >> String.concat >> Element.text >> Element.el [ Element.centerY ]
                      }
                    , { header = "Year" |> toHeader
                      , width = Element.fill |> Element.maximum 80
                      , view = .year >> String.fromInt >> Element.text >> Element.el [ Element.centerY ]
                      }
                    , { header = "Details" |> toHeader
                      , width = Element.fill |> Element.maximum 60
                      , view =
                            \class ->
                                Input.button
                                    [ Background.color Colors.theme.a400
                                    , Border.width 1
                                    , Border.rounded 3
                                    , Element.paddingXY 10 2
                                    , Element.mouseOver [ Background.color Colors.theme.a200 ]
                                    ]
                                    { label = Element.text "More" |> Element.el [ Element.centerX ]
                                    , onPress = Just (ToDetails class.id)
                                    }
                      }
                    ]
                , data = classList
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
        , Element.spacing 10
        ]
        [ viewClassFilters model.filtersForm model.filters
        , blankIfAbsent Paged.viewPagination model.data |> Element.map PaginationChanged
        , viewData model.data
        , blankIfAbsent Paged.viewPagination model.data |> Element.map PaginationChanged
        ]
