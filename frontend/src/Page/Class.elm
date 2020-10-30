module Page.Class exposing (Model, Msg, getPageLink, getPageTitle, init, update, view, viewSessions)

import Api
import Base64
import Browser.Navigation as Navigation
import Class exposing (Class, ClassId, ClassSession, ClassTutor)
import Colors
import Date
import DatePicker exposing (Model)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
import Json.Decode as Decode
import Page.Tutor
import RemoteData exposing (WebData)
import Styles
import Task
import Tutor
import Url.Builder as Builder
import Utils


type alias NewSessionForm =
    { datePicker : DatePicker.Model
    , datePickerText : String
    , date : Maybe Date.Date
    , remarks : String
    , duration : Float
    , display : Bool
    , errorMessage : Maybe String
    }


emptyForm : NewSessionForm
emptyForm =
    { datePicker = DatePicker.init
    , datePickerText = ""
    , date = Nothing
    , remarks = ""
    , duration = 3
    , display = False
    , errorMessage = Nothing
    }


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : ClassId
    , data : WebData Class
    , sessions : WebData (List ClassSession)
    , tutors : WebData (List ClassTutor)
    , today : Maybe Date.Date
    , form : NewSessionForm
    , hoveredSession : Int
    , hoveredTutor : Int
    }


type Msg
    = GotClassData (Result Http.Error Class)
    | GotSessionsData (Result Http.Error (List ClassSession))
    | GotTutorsData (Result Http.Error (List ClassTutor))
    | GotNewSession (Result Http.Error String)
    | GotToday Date.Date
    | NavigateToTutor String
    | NavigateToAddTutors
    | NavigateToTakeAttendance String
    | NavigateToEdit
    | DisplayAddSession
    | FormPickerChanged DatePicker.ChangeEvent
    | RemarksEntered String
    | DurationEntered Float
    | SubmitNewSession
    | HoverChangedTutor Int
    | HoverChangedSession Int


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.data |> Maybe.map .name |> Maybe.withDefault ("Class ID: " ++ model.id)


getPageLink : Class.ClassId -> String
getPageLink id =
    Builder.absolute [ "class", id ] []


fetchClassData : Api.Credentials -> ClassId -> Cmd Msg
fetchClassData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "class", id ] []
        , expect = Http.expectJson GotClassData Class.classDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTutorData : Api.Credentials -> ClassId -> Cmd Msg
fetchTutorData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "class", id, "tutors" ] []
        , expect = Http.expectJson GotTutorsData (Decode.list Class.classTutorDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchSessionsData : Api.Credentials -> ClassId -> Cmd Msg
fetchSessionsData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "class", id, "sessions" ] []
        , expect = Http.expectJson GotSessionsData (Decode.list Class.classSessionDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


postNewSession : Api.Credentials -> ClassId -> ClassSession -> Cmd Msg
postNewSession credentials id session =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Class.classSessionEncoder session)
        , url = Builder.crossOrigin Api.endpoint [ "class", id, "addsession" ] []
        , expect = Http.expectJson GotNewSession (Decode.field "id" Decode.string)
        , timeout = Nothing
        , tracker = Nothing
        }


init : ClassId -> Api.Credentials -> Navigation.Key -> ( Model, Cmd Msg )
init id credentials key =
    let
        model =
            { key = key
            , credentials = credentials
            , id = id
            , data = RemoteData.Loading
            , sessions = RemoteData.Loading
            , tutors = RemoteData.Loading
            , today = Nothing
            , form = emptyForm
            , hoveredSession = -1
            , hoveredTutor = -1
            }
    in
    ( model
    , Cmd.batch
        [ fetchClassData model.credentials model.id
        , fetchSessionsData model.credentials model.id
        , fetchTutorData model.credentials model.id
        , Task.perform GotToday Date.today
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newForm =
            model.form
    in
    case msg of
        GotClassData result ->
            ( { model
                | data = RemoteData.fromResult result
                , form = { newForm | duration = Result.toMaybe result |> Maybe.map .duration |> Maybe.withDefault 3.0 }
              }
            , Cmd.none
            )

        GotSessionsData result ->
            ( { model | sessions = RemoteData.fromResult result }, Cmd.none )

        GotTutorsData result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        GotToday today ->
            ( { model
                | form =
                    { newForm
                        | date = Just today
                        , datePicker = newForm.datePicker |> DatePicker.setToday today
                    }
                , today = Just today
              }
            , Cmd.none
            )

        NavigateToTutor id ->
            ( model, Navigation.pushUrl model.key (Page.Tutor.getPageLink id) )

        NavigateToAddTutors ->
            ( model, Navigation.pushUrl model.key (getPageLink model.id ++ "/addtutor") )

        NavigateToTakeAttendance sessionId ->
            ( model, Navigation.pushUrl model.key (getPageLink model.id ++ "/session/" ++ sessionId) )

        NavigateToEdit ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "class", model.id, "edit" ] []) )

        DisplayAddSession ->
            ( { model | form = { newForm | display = not newForm.display } }, Cmd.none )

        RemarksEntered remark ->
            ( { model | form = { newForm | remarks = remark } }, Cmd.none )

        DurationEntered duration ->
            ( { model | form = { newForm | duration = duration } }, Cmd.none )

        HoverChangedSession value ->
            ( { model | hoveredSession = value }, Cmd.none )

        HoverChangedTutor value ->
            ( { model | hoveredTutor = value }, Cmd.none )

        SubmitNewSession ->
            case newForm.date of
                Nothing ->
                    ( { model | form = { newForm | errorMessage = Just "Date must be provided." } }, Cmd.none )

                Just date ->
                    ( model
                    , postNewSession model.credentials
                        model.id
                        { date = date
                        , duration = newForm.duration
                        , remarks = newForm.remarks
                        , id = ""
                        }
                    )

        GotNewSession result ->
            ( model, Cmd.none )

        FormPickerChanged change ->
            case change of
                DatePicker.TextChanged text ->
                    case Date.fromIsoString text of
                        Ok date ->
                            ( { model
                                | form =
                                    { newForm
                                        | date = Just date
                                        , datePickerText = text
                                        , datePicker = newForm.datePicker |> DatePicker.setVisibleMonth date
                                    }
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | form = { newForm | datePickerText = text } }, Cmd.none )

                DatePicker.PickerChanged event ->
                    ( { model
                        | form =
                            { newForm
                                | datePicker = newForm.datePicker |> DatePicker.update event
                            }
                      }
                    , Cmd.none
                    )

                DatePicker.DateChanged date ->
                    ( { model
                        | form =
                            { newForm
                                | date = Just date
                                , datePickerText = Date.toIsoString date
                                , datePicker = newForm.datePicker |> DatePicker.setVisibleMonth date
                            }
                      }
                    , Cmd.none
                    )


viewRow : String -> Class -> (Class -> String) -> Element Msg
viewRow label tutor accessor =
    Element.row
        []
        [ Element.text label |> Element.el [ Font.bold, Element.width (Element.px 150) ]
        , Element.text (accessor tutor)
        ]


viewDetails : Class -> Element Msg
viewDetails class =
    Element.column
        [ Element.padding 20
        , Element.spacing 10
        , Element.width Element.fill
        , Background.color Colors.theme.p50
        ]
        [ Element.row [ Element.spacing 20 ]
            [ Element.text "Class Details" |> Element.el [ Font.size 16, Font.bold ]
            , Input.button
                Styles.buttonStyleCozy
                { onPress = Just NavigateToEdit, label = Element.text "Edit" |> Element.el [ Element.centerX ] }
            ]
        , Element.el [ Element.height (Element.px 5) ] Element.none
        , viewRow "Name" class .name
        , viewRow "Year" class (.year >> String.fromInt)
        , viewRow "Timeslot" class .timeslot
        , viewRow "Days" class (.days >> List.map Utils.daysToString >> String.join ", ")
        , viewRow "Duration" class (.duration >> String.fromFloat)
        ]


viewNewSessionForm : NewSessionForm -> Element Msg
viewNewSessionForm form =
    if form.display then
        Element.column
            [ Element.spacing 5
            , Element.paddingXY 30 20
            , Border.width 1
            , Border.rounded 3
            , Background.color (Element.rgb255 255 255 255)
            ]
            [ Element.text "Create New Session" |> Element.el [ Font.bold ]
            , Element.el [ Element.height (Element.px 5) ] Element.none
            , DatePicker.input Styles.dateFieldStyle
                { label = Element.text "Date" |> Input.labelLeft Styles.textLabelStyle
                , model = form.datePicker
                , onChange = FormPickerChanged
                , placeholder = Nothing
                , selected = form.date
                , settings = DatePicker.defaultSettings
                , text = form.datePickerText
                }
            , Element.row
                [ Element.spacing 5
                ]
                [ Element.text "Remark" |> Element.el Styles.textLabelStyle
                , Input.text
                    Styles.textFieldStyle
                    { onChange = RemarksEntered
                    , text = form.remarks
                    , placeholder = Element.text "Enter or choose from below" |> Input.placeholder [] |> Just
                    , label = Input.labelHidden "Remarks"
                    }
                ]
            , Element.row
                [ Element.spacing 5 ]
                [ Element.el Styles.textLabelStyle Element.none
                , Input.button Styles.buttonStyleCozy
                    { onPress = Just (RemarksEntered "Class"), label = Element.text "Class" }
                , Input.button Styles.buttonStyleCozy
                    { onPress = Just (RemarksEntered "Public Holiday"), label = Element.text "Public Holiday" }
                , Input.button Styles.buttonStyleCozy
                    { onPress = Just (RemarksEntered "Cancelled (Enter reason)"), label = Element.text "Cancelled" }
                ]
            , Element.row [ Element.spacing 20, Element.width Element.fill ]
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
                    { onChange = DurationEntered
                    , label = Element.text "Duration" |> Input.labelLeft Styles.textLabelStyle
                    , min = 0
                    , max = 10
                    , value = form.duration
                    , thumb = Input.defaultThumb
                    , step = Just 0.5
                    }
                , Element.text (String.fromFloat form.duration) |> Element.el [ Element.width (Element.px 25) ]
                , Element.text "Hours" |> Element.el [ Element.alignRight ]
                ]
            , Element.el [ Element.height (Element.px 5) ] Element.none
            , case form.errorMessage of
                Nothing ->
                    Element.none

                Just message ->
                    Element.text message |> Element.el [ Font.color Colors.red ]
            , Input.button (Element.alignRight :: Styles.buttonStyleComfy)
                { onPress = Just SubmitNewSession
                , label = Element.text "Submit" |> Element.el [ Element.centerX ]
                }
            ]

    else
        Element.none


viewCalendarDay : Date.Date -> List ClassSession -> Date.Date -> Element Msg
viewCalendarDay today sessions date =
    let
        text =
            (date |> Date.day |> String.fromInt) ++ Utils.ifElse (" " ++ Date.format "MMM" date) "" (Date.day date == 1)

        sessionsOnDate =
            List.filter (.date >> (==) date) sessions
    in
    text
        |> Element.text
        |> Element.el
            []
        |> Element.el
            (List.concat
                [ -- Today marker
                  Utils.ifElse
                    [ Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Border.rounded 10
                            , Border.width 1
                            ]
                            Element.none
                        )
                    ]
                    []
                    (date == today)
                , -- Session marker
                  Utils.ifElse
                    []
                    [ Element.inFront
                        (Input.button
                            [ Background.color Colors.theme.p700
                            , Element.width Element.fill
                            , Element.height Element.fill
                            , Border.rounded 10
                            , Element.paddingEach { top = 5, bottom = 5, left = 8, right = 0 }
                            ]
                            { label = Element.text text
                            , onPress = Nothing
                            }
                        )
                    , Font.color Colors.white
                    , Font.bold
                    ]
                    (List.isEmpty sessionsOnDate)
                , -- Other attributes
                  [ Background.color
                        (Utils.ifElse
                            Colors.theme.p200
                            Colors.theme.p50
                            ((date |> Date.monthNumber |> Basics.modBy 2) == 0)
                        )
                  , Element.width (Element.fill |> Element.minimum 40)
                  , Element.paddingEach { top = 5, bottom = 5, left = 8, right = 0 }
                  , Element.htmlAttribute (Html.Attributes.title (Date.format "dd MMMM YYYY" date))
                  ]
                ]
            )


viewCalendarWeek : Date.Date -> Date.Date -> List ClassSession -> Element Msg
viewCalendarWeek today weekStart sessions =
    let
        year =
            Date.year weekStart

        weekNo =
            Date.weekNumber weekStart
    in
    Element.column
        [ Element.paddingXY 0 10 ]
        (Utils.allDays
            |> List.map (Date.fromWeekDate year weekNo)
            |> List.map (viewCalendarDay today sessions)
        )


viewSessionsCalendar : Date.Date -> List ClassSession -> Element Msg
viewSessionsCalendar today sessions =
    Element.wrappedRow
        []
        (Element.column
            [ Element.paddingXY 0 10 ]
            (Utils.allDays
                |> List.map
                    (Utils.daysToString
                        >> Element.text
                        >> Element.el
                            [ Element.paddingEach
                                { top = 5
                                , bottom = 5
                                , left = 0
                                , right = 10
                                }
                            , Font.bold
                            ]
                    )
            )
            :: (List.range 0 20
                    |> List.reverse
                    |> List.map
                        (\before ->
                            viewCalendarWeek
                                today
                                (Date.toRataDie today - 7 * before |> Date.fromRataDie)
                                sessions
                        )
               )
        )


viewSessions : Int -> Maybe Date.Date -> NewSessionForm -> List ClassSession -> Element Msg
viewSessions hovered maybeToday form sessions =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text
                |> Element.text
                |> Element.el [ Font.bold, Element.padding 4 ]
                |> Element.el [ Element.paddingXY 0 4 ]

        cell =
            Utils.cell HoverChangedSession (.id >> NavigateToTakeAttendance) hovered
    in
    Element.column
        [ Element.padding 20
        , Element.spacing 20
        , Element.width Element.fill
        , Background.color Colors.theme.p50
        ]
        [ Element.row [ Element.spacing 20, Element.width Element.fill ]
            [ Element.text "Sessions" |> Element.el [ Font.size 16, Font.bold ]
            , Input.button
                Styles.buttonStyleCozy
                { onPress = Just DisplayAddSession
                , label =
                    if form.display then
                        Element.text "Hide ▲" |> Element.el [ Element.width Element.shrink ]

                    else
                        Element.text "Add New Session ▼" |> Element.el [ Element.width Element.shrink ]
                }
            ]
        , viewNewSessionForm form
        , Element.indexedTable
            [ Element.width Element.fill
            ]
            { data = sessions
            , columns =
                [ { header = "Date" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = .date >> Date.toIsoString >> Element.text |> cell
                  }
                , { header = "Duration" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = .duration >> String.fromFloat >> Element.text |> cell
                  }
                , { header = "Remarks" |> toHeader
                  , width = Element.fill |> Element.maximum 300
                  , view = .remarks >> Element.text |> cell
                  }
                , { header = "Attendance" |> toHeader
                  , width = Element.fill |> Element.maximum 50
                  , view =
                        (\sess ->
                            Input.button
                                [ Background.color Colors.theme.a400
                                , Border.width 1
                                , Border.rounded 3
                                , Element.paddingXY 10 2
                                , Element.mouseOver [ Background.color Colors.theme.a200 ]
                                ]
                                { onPress = Just (NavigateToTakeAttendance sess.id)
                                , label = Element.el [ Element.centerX ] (Element.text "Take")
                                }
                        )
                            |> cell
                  }
                ]
            }
        , case maybeToday of
            Nothing ->
                Element.none

            Just today ->
                viewSessionsCalendar today sessions
        ]


viewTutors : Int -> List ClassTutor -> Element Msg
viewTutors hovered tutors =
    Element.column
        [ Element.padding 20
        , Element.spacing 20
        , Element.width Element.fill
        , Background.color Colors.theme.p50
        ]
        [ Element.row [ Element.spacing 20, Element.width Element.fill ]
            [ Element.text "Tutors" |> Element.el [ Font.size 16, Font.bold ]
            , Input.button
                Styles.buttonStyleCozy
                { onPress = Just NavigateToAddTutors, label = Element.text "Add New Tutor" }
            ]
        , let
            toHeader : String -> Element Msg
            toHeader text =
                text
                    |> Element.text
                    |> Element.el [ Font.bold, Element.padding 4 ]
                    |> Element.el [ Element.paddingXY 0 4 ]

            cell =
                Utils.cell HoverChangedTutor (.id >> NavigateToTutor) hovered
          in
          Element.indexedTable
            []
            { data = tutors
            , columns =
                [ { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 200
                  , view = .name >> Element.text |> cell
                  }
                , { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 200
                  , view = .admin >> Tutor.adminLevelAsString >> Element.text |> cell
                  }
                , { header = "Join Class Date" |> toHeader
                  , width = Element.fill |> Element.maximum 140
                  , view = .joinDate >> Date.toIsoString >> Element.text |> cell
                  }
                , { header = "Details" |> toHeader
                  , width = Element.fill |> Element.maximum 60
                  , view =
                        (\t ->
                            Input.button
                                Styles.buttonStyleCozy
                                { onPress = Just (NavigateToTutor t.id)
                                , label = Element.text "More" |> Element.el [ Element.centerX ]
                                }
                        )
                            |> cell
                  }
                ]
            }
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing 10
        , Element.height Element.fill
        , Element.width Element.fill
        ]
        [ Utils.viewWebData viewDetails model.data
        , Utils.viewWebData (viewSessions model.hoveredSession model.today model.form) model.sessions
        , Utils.viewWebData (viewTutors model.hoveredTutor) model.tutors
        ]
