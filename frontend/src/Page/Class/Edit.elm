module Page.Class.Edit exposing
    ( Model
    , Msg
    , getPageLink
    , getPageTitle
    , initWithClass
    , initWithEmpty
    , update
    , view
    )

import Api exposing (Credentials)
import Base64
import Browser.Navigation as Navigation
import Class exposing (Class)
import Colors
import Date
import DatePicker
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Maybe.Extra
import Regex
import RemoteData exposing (WebData)
import Styles
import Task
import Time
import Url.Builder as Builder
import Utils


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Maybe Class.ClassId
    , data : Class.Class
    , formState : FormState
    , errorMessage : Maybe String
    , successMessage : Maybe String
    , today : Date.Date
    }


type Msg
    = NameChanged String
    | TimeslotChanged String
    | DayChanged Time.Weekday
    | DurationChanged Float
    | YearChanged Int
    | ActiveChanged
    | SetToday Date.Date
    | ToClass
    | Submit
    | GotClassData (Result Http.Error Class.Class)
    | GotNewClassResult (Result Http.Error Class.ClassId)
    | GotEditClassResult (Result Http.Error ())


type alias FormState =
    ()


getPageTitle : Model -> String
getPageTitle model =
    case model.id of
        Nothing ->
            "New Class"

        Just classId ->
            "Edit: " ++ model.data.name


getPageLink : Model -> String
getPageLink model =
    case model.id of
        Nothing ->
            Builder.absolute [ "classes", "new" ] []

        Just classId ->
            Builder.absolute [ "class", classId, "edit" ] []


postNewClass : Api.Credentials -> Class.Class -> Cmd Msg
postNewClass credentials data =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Class.classEncoder data)
        , url = Builder.crossOrigin Api.endpoint [ "classes", "new" ] []
        , expect = Http.expectJson GotNewClassResult (Decode.field "id" Decode.string)
        , timeout = Nothing
        , tracker = Nothing
        }


postClassUpdate : Api.Credentials -> Class.ClassId -> Class.Class -> Cmd Msg
postClassUpdate credentials classId data =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Class.classEncoder data)
        , url = Builder.crossOrigin Api.endpoint [ "class", classId ] []
        , expect = Http.expectWhatever GotEditClassResult
        , timeout = Nothing
        , tracker = Nothing
        }


initWithEmpty : Api.Credentials -> Navigation.Key -> ( Model, Cmd Msg )
initWithEmpty credentials key =
    ( { key = key
      , credentials = credentials
      , id = Nothing
      , data = Class.emptyClass
      , formState = ()
      , errorMessage = Nothing
      , successMessage = Nothing
      , today = Date.fromRataDie 1
      }
    , Task.perform SetToday Date.today
    )


initWithClass : Api.Credentials -> Navigation.Key -> Class.ClassId -> ( Model, Cmd Msg )
initWithClass credentials key id =
    ( { key = key
      , credentials = credentials
      , id = Just id
      , data = Class.emptyClass
      , formState = ()
      , errorMessage = Nothing
      , successMessage = Nothing
      , today = Date.fromRataDie 1
      }
    , Cmd.batch
        [ Http.request
            { method = "GET"
            , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
            , body = Http.emptyBody
            , url = Builder.crossOrigin Api.endpoint [ "class", id ] []
            , expect = Http.expectJson GotClassData Class.classDecoder
            , timeout = Nothing
            , tracker = Nothing
            }
        , Task.perform SetToday Date.today
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
    case msg of
        NameChanged value ->
            ( { model | data = { data | name = value } }, Cmd.none )

        TimeslotChanged value ->
            ( { model | data = { data | timeslot = value } }, Cmd.none )

        DayChanged value ->
            let
                days =
                    if List.member value data.days then
                        List.filter ((/=) value) data.days

                    else
                        value :: data.days
            in
            ( { model | data = { data | days = days } }, Cmd.none )

        YearChanged value ->
            ( { model | data = { data | year = value } }, Cmd.none )

        DurationChanged value ->
            ( { model | data = { data | duration = value } }, Cmd.none )

        ActiveChanged ->
            ( { model | data = { data | active = Basics.not data.active } }, Cmd.none )

        SetToday today ->
            ( { model
                | data =
                    { data
                        | year =
                            if data.year == 0 then
                                Date.year today

                            else
                                data.year
                    }
                , today = today
              }
            , Cmd.none
            )

        ToClass ->
            case model.id of
                Nothing ->
                    ( model, Navigation.pushUrl model.key (Builder.absolute [ "classes" ] []) )

                Just id ->
                    ( model, Navigation.pushUrl model.key (Builder.absolute [ "class", id ] []) )

        Submit ->
            case model.id of
                Nothing ->
                    ( model, postNewClass model.credentials model.data )

                Just id ->
                    ( model, postClassUpdate model.credentials id model.data )

        GotClassData result ->
            ( { model
                | data =
                    Result.toMaybe result
                        |> Maybe.withDefault Class.emptyClass
              }
            , Cmd.none
            )

        GotNewClassResult result ->
            case result of
                Ok id ->
                    ( { model | id = Just id, successMessage = Just "Added successfully." }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (Api.errorToString error) }, Cmd.none )

        GotEditClassResult result ->
            case result of
                Ok _ ->
                    ( { model | successMessage = Just "Updated successfully." }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (Api.errorToString error) }, Cmd.none )


viewValidation : Bool -> Element Msg
viewValidation validated =
    let
        color =
            if validated then
                Colors.green

            else
                Colors.red
    in
    Element.el
        [ Element.height Element.fill
        , Element.width (Element.px 20)
        , Element.padding 5
        ]
        (Element.el
            [ Background.color color
            , Element.height Element.fill
            , Element.width Element.fill
            , Border.rounded 50
            ]
            Element.none
        )


viewRow : String -> Class -> (Class -> String) -> (String -> Msg) -> (String -> Bool) -> Element Msg
viewRow label data accessor updateMsg validator =
    Element.wrappedRow
        []
        [ Input.text Styles.textFieldStyle
            { label = Element.text label |> Input.labelLeft Styles.textLabelStyle
            , onChange = updateMsg
            , placeholder = Nothing
            , text = accessor data
            }
        , viewValidation (validator (accessor data))
        ]


viewDurationSlider : Float -> Element Msg
viewDurationSlider duration =
    Element.row [ Element.spacing 20, Element.width Element.fill ]
        [ Input.slider
            [ Element.width Element.fill
            , Element.centerY
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
            { onChange = DurationChanged
            , label = Element.text "Duration" |> Input.labelLeft Styles.textLabelStyle
            , min = 0
            , max = 10
            , value = duration
            , thumb = Input.defaultThumb
            , step = Just 0.5
            }
        , Element.text (String.fromFloat duration) |> Element.el [ Element.width (Element.px 25) ]
        , Element.text "Hours" |> Element.el [ Element.alignRight ]
        ]


viewYearSlider : Date.Date -> Int -> Element Msg
viewYearSlider today year =
    let
        midYear =
            Date.year today
    in
    Element.row [ Element.spacing 20, Element.width Element.fill ]
        [ Input.slider
            [ Element.width Element.fill
            , Element.centerY
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
            { onChange = Basics.floor >> YearChanged
            , label = Element.text "Year" |> Input.labelLeft Styles.textLabelStyle
            , min = midYear - 2 |> Basics.toFloat
            , max = midYear + 3 |> Basics.toFloat
            , value = year |> Basics.toFloat
            , step = Just 1
            , thumb = Input.defaultThumb
            }
        , Element.text (String.fromInt year) |> Element.el [ Element.width (Element.px 25) ]
        ]


viewToggleActive : Bool -> Element Msg
viewToggleActive isActive =
    let
        activeGreen =
            Element.rgb 0 255 0

        inactiveWhite =
            Colors.white

        backgroundColor : Bool -> Element.Color
        backgroundColor active =
            if active then
                activeGreen

            else
                inactiveWhite

        fontColor : Bool -> Element.Color
        fontColor active =
            if active then
                Colors.black

            else
                Colors.grey
    in
    Element.row
        [ Element.spacing 5 ]
        [ Element.text "Status" |> Element.el Styles.textLabelStyle
        , Element.row
            [ Element.spacing 5 ]
            [ Input.button
                [ Background.color (backgroundColor isActive)
                , Font.color (fontColor isActive)
                , Element.paddingXY 5 2
                , Border.rounded 3
                , Border.width 1
                ]
                { label = Element.text "Active"
                , onPress = Just ActiveChanged
                }
            , Input.button
                [ Background.color (backgroundColor (Basics.not isActive))
                , Font.color (fontColor (Basics.not isActive))
                , Element.paddingXY 5 2
                , Border.rounded 3
                , Border.width 1
                ]
                { label = Element.text "Inactive"
                , onPress = Just ActiveChanged
                }
            ]
        ]


viewToggleDays : List Time.Weekday -> List Time.Weekday -> Element Msg
viewToggleDays all selectedDays =
    let
        activeGreen =
            Element.rgb 0 255 0

        inactiveWhite =
            Colors.white

        backgroundColor : Bool -> Element.Color
        backgroundColor active =
            if active then
                activeGreen

            else
                inactiveWhite

        fontColor : Bool -> Element.Color
        fontColor active =
            if active then
                Colors.black

            else
                Colors.grey
    in
    Element.row
        [ Element.spacing 5 ]
        [ Element.text "Days" |> Element.el Styles.textLabelStyle
        , Element.row [ Element.spacing 5 ]
            (List.map
                (\day ->
                    Input.button
                        [ Background.color (backgroundColor (List.member day selectedDays))
                        , Font.color (fontColor (List.member day selectedDays))
                        , Element.paddingXY 5 2
                        , Border.rounded 3
                        , Border.width 1
                        ]
                        { label = Utils.daysToString day |> Element.text
                        , onPress = Just (DayChanged day)
                        }
                )
                all
            )
        ]


viewForm : Date.Date -> Class.Class -> Element Msg
viewForm today class =
    Element.column
        [ Element.spacing 10 ]
        [ viewRow "Name" class .name NameChanged (String.isEmpty >> Basics.not)
        , viewRow "Timeslot" class .timeslot TimeslotChanged (Basics.always Basics.True)
        , viewToggleDays Utils.allDays class.days
        , viewYearSlider today class.year
        , viewDurationSlider class.duration
        , viewToggleActive class.active
        ]


view : Model -> Element Msg
view model =
    Element.el
        [ Element.padding 20
        , Element.width Element.fill
        ]
        (Element.column
            [ Element.padding 20
            , Element.width Element.fill
            , Element.height Element.fill
            , Background.color Colors.theme.p50
            , Element.spacing 20
            ]
            [ viewForm model.today model.data
            , Input.button
                [ Background.color Colors.theme.a400
                , Border.width 1
                , Border.rounded 3
                , Element.paddingXY 10 2
                , Element.mouseOver [ Background.color Colors.theme.a200 ]
                ]
                { onPress = Just Submit
                , label = Element.text "Submit"
                }
            , case model.errorMessage of
                Nothing ->
                    Element.none

                Just message ->
                    Element.text message |> Element.el [ Font.color Colors.red, Font.bold ]
            , case model.successMessage of
                Nothing ->
                    Element.none

                Just message ->
                    Element.row [ Element.spacing 30 ]
                        [ Element.text message |> Element.el [ Font.color Colors.theme.p600, Font.bold ]
                        , Input.button Styles.buttonStyleWide
                            { onPress = Just ToClass
                            , label = Element.text "Back to profile" |> Element.el [ Element.centerX ]
                            }
                        ]
            ]
        )
