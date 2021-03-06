module Page.Tutor exposing
    ( Model
    , Msg
    , getPageLink
    , getPageTitle
    , init
    , update
    , view
    )

import Api
import Base64
import Browser.Navigation as Navigation
import Class exposing (Class, ClassSession, ClassTutor)
import Colors
import Date
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData, WebData, withDefault)
import Styles
import Task
import Tutor exposing (Tutor, TutorExtended, TutorId, tutorDecoder)
import Url.Builder as Builder
import Utils


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Tutor.TutorId
    , today : Maybe Date.Date
    , tutorData : WebData Tutor
    , tutorExtendedData : WebData TutorExtended
    , classData : WebData (List Class)
    , sessionData : WebData (Dict Class.ClassId (List ClassSession))
    , tutorHours : WebData (Dict Class.ClassId Float)
    , hoveredClass : Int
    }


type Msg
    = GotTutorData (Result Http.Error Tutor)
    | GotClassData (Result Http.Error (List Class))
    | GotSessionData (Result Http.Error (Dict Class.ClassId (List ClassSession)))
    | GotTutorHours (Result Http.Error (Dict Class.ClassId Float))
    | GotTutorExtendedData (Result Http.Error TutorExtended)
    | GotToday Date.Date
    | ToEditDetails
    | ToClassDetails Class.ClassId
    | HoveredChangedClass Int
    | HoveredChangedSession Int


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.tutorData |> Maybe.map .name |> Maybe.withDefault "Tutor"


getPageLink : String -> String
getPageLink id =
    Builder.absolute [ "tutor", id ] []


init : Api.Credentials -> Navigation.Key -> String -> ( Model, Cmd Msg )
init credentials key id =
    ( { key = key
      , credentials = credentials
      , id = id
      , tutorData = RemoteData.Loading
      , tutorExtendedData = RemoteData.NotAsked
      , classData = RemoteData.Loading
      , sessionData = RemoteData.Loading
      , tutorHours = RemoteData.Loading
      , hoveredClass = -1
      , today = Nothing
      }
    , Cmd.batch
        [ fetchTutorData credentials id
        , fetchClassData credentials id
        , fetchSessionData credentials id
        , fetchTutorHours credentials id
        , fetchTutorExtendedData credentials id
        , Task.perform GotToday Date.today
        ]
    )


fetchTutorData : Api.Credentials -> TutorId -> Cmd Msg
fetchTutorData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id ] []
        , expect = Http.expectJson GotTutorData tutorDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTutorExtendedData : Api.Credentials -> TutorId -> Cmd Msg
fetchTutorExtendedData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id, "extended" ] []
        , expect = Http.expectJson GotTutorExtendedData Tutor.tutorExtendedDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchClassData : Api.Credentials -> TutorId -> Cmd Msg
fetchClassData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id, "classes" ] []
        , expect = Http.expectJson GotClassData (Decode.list Class.classDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchSessionData : Api.Credentials -> TutorId -> Cmd Msg
fetchSessionData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id, "attended" ] []
        , expect = Http.expectJson GotSessionData (Decode.dict (Decode.list Class.classSessionDecoder))
        , timeout = Nothing
        , tracker = Nothing
        }


fetchTutorHours : Api.Credentials -> TutorId -> Cmd Msg
fetchTutorHours credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id, "hours" ] []
        , expect = Http.expectJson GotTutorHours (Decode.dict Decode.float)
        , timeout = Nothing
        , tracker = Nothing
        }


viewRow : String -> String -> Element Msg
viewRow label value =
    Element.row
        []
        [ Element.text label |> Element.el [ Element.width (Element.px 150), Font.bold ]
        , Element.text value |> Element.el []
        ]


viewOptionsRow : String -> List a -> List ( a, String ) -> Element Msg
viewOptionsRow label selected labels =
    let
        all =
            List.map Tuple.first labels

        getLabel e =
            List.filter (Tuple.first >> (==) e) labels
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault "<Invalid>"
    in
    Element.row
        []
        [ Element.text label |> Element.el [ Element.width (Element.px 150), Font.bold ]
        , Element.row [ Element.spacing 5 ]
            (List.map
                (\elem ->
                    Input.button
                        (Styles.optionButtonStyle (List.member elem selected))
                        { label = getLabel elem |> Element.text, onPress = Nothing }
                )
                all
            )
        ]


viewDetails : Tutor -> Element Msg
viewDetails data =
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 10
        ]
        [ Element.row [ Element.spacing 20 ]
            [ Element.text "Tutor Details" |> Element.el [ Font.size 16, Font.bold ]
            , Input.button
                Styles.buttonStyleComfy
                { onPress = Just ToEditDetails, label = Element.text "Edit" |> Element.el [ Element.centerX ] }
            ]
        , Element.el [ Element.height (Element.px 10) ] Element.none
        , viewRow "Name" data.name
        , viewRow "Email" data.email
        , viewRow "School" data.school
        , viewRow "Status" (data.status |> Tutor.tutorStatusAsString)
        , viewRow "Admin Level" (data.admin |> Tutor.adminLevelAsString)
        , viewRow "Gender" (data.gender |> Utils.genderToString)
        , viewRow "Date of Birth" (data.dateOfBirth |> Date.format "d MMM y")
        , viewRow "Start date" (data.dateOfRegistration |> Date.format "d MMM y")
        ]


viewRemarks : List String -> Element Msg
viewRemarks remarks =
    Element.row
        []
        [ Element.text "Remarks" |> Element.el [ Element.width (Element.px 150), Font.bold ]
        , Element.column []
            (List.map
                Element.text
                remarks
            )
        ]


viewExtendedDetails : TutorExtended -> Element Msg
viewExtendedDetails tutor =
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 10
        ]
        [ viewOptionsRow "Languages spoken" tutor.languages (Tutor.allLanguages |> List.map (\x -> ( x, Tutor.tutorLanguageAsString x )))
        , viewOptionsRow "Available days" tutor.available (Utils.allDays |> List.map (\x -> ( x, Utils.daysToString x )))
        , viewOptionsRow "Subjects keen" tutor.subjects (Tutor.allSubjects |> List.map (\x -> ( x, Tutor.subjectAsString x )))
        , viewRow "Career goal" tutor.careerGoal
        , viewRow "School type" (tutor.schoolType |> Tutor.schoolTypeAsString)
        , viewRow "Year of Graduation" (tutor.yearOfGraduation |> String.fromInt)
        , viewRemarks tutor.remarks
        ]


viewClasses : Int -> Dict Class.ClassId Float -> List Class.Class -> Element Msg
viewClasses hovered hours classes =
    let
        toHeader =
            Utils.toHeader

        cell =
            Utils.cell HoveredChangedClass (Just (.id >> ToClassDetails)) hovered
    in
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.text "Classes" |> Element.el [ Font.size 16, Font.bold ]
        , Element.indexedTable
            []
            { columns =
                [ { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 200
                  , view = .name >> Element.text >> Element.el [ Element.centerY ] |> cell
                  }
                , { header = "Days" |> toHeader
                  , width = Element.fill |> Element.maximum 150
                  , view =
                        .days
                            >> List.map Utils.daysToString
                            >> List.intersperse ", "
                            >> String.concat
                            >> Element.text
                            >> Element.el [ Element.centerY ]
                            |> cell
                  }
                , { header = "Year" |> toHeader
                  , width = Element.fill |> Element.maximum 80
                  , view =
                        .year
                            >> String.fromInt
                            >> Element.text
                            >> Element.el [ Element.centerY ]
                            |> cell
                  }
                , { header = "Hours" |> toHeader
                  , width = Element.fill |> Element.maximum 60
                  , view =
                        (\class ->
                            Dict.get class.id hours
                                |> Maybe.map String.fromFloat
                                |> Maybe.withDefault ""
                        )
                            >> Element.text
                            >> Element.el [ Element.centerY ]
                            |> cell
                  }
                , { header = "Details" |> toHeader
                  , width = Element.fill |> Element.maximum 60
                  , view =
                        (\class ->
                            Input.button
                                Styles.buttonStyleCozy
                                { label = Element.text "More" |> Element.el [ Element.centerX ]
                                , onPress = Just (ToClassDetails class.id)
                                }
                        )
                            |> cell
                  }
                ]
            , data = classes
            }
        ]


viewCalendarDay : Date.Date -> List ClassSession -> Date.Date -> Element Msg
viewCalendarDay today sessions date =
    let
        text =
            Element.row
                []
                [ date |> Date.day |> String.fromInt |> Element.text
                , Utils.ifElse (" " ++ Date.format "MMM" date) "" (Date.day date == 1)
                    |> Element.text
                    |> Element.el [ Font.size 10, Element.alignBottom ]
                ]

        sessionsOnDate =
            List.filter (.date >> (==) date) sessions
    in
    text
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
                            , Border.color Colors.black
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
                            , Border.width (Utils.ifElse 1 0 (date == today))
                            , Border.color Colors.black
                            ]
                            { label = text
                            , onPress = Nothing
                            }
                        )
                    , Font.color Colors.white
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


viewRecentSessionsCalendar : Date.Date -> List Class.Class -> List ( Class.ClassId, Class.ClassSession ) -> Element Msg
viewRecentSessionsCalendar today classes sessions =
    let
        headings =
            Utils.allDays
                |> List.map Utils.daysToString
                |> List.map Element.text
                |> List.map
                    (Element.el
                        [ Element.paddingEach
                            { top = 5
                            , bottom = 5
                            , left = 0
                            , right = 10
                            }
                        , Font.bold
                        ]
                    )
                |> Element.column
                    [ Element.paddingXY 0 10 ]
    in
    Element.wrappedRow
        [ Border.widthEach
            { bottom = 1
            , top = 1
            , left = 0
            , right = 0
            }
        ]
        (headings
            :: (List.range 0 20
                    |> List.reverse
                    |> List.map
                        (\before ->
                            viewCalendarWeek
                                today
                                (Date.toRataDie today - 7 * before |> Date.fromRataDie)
                                (sessions |> List.map Tuple.second)
                        )
               )
        )
        |> Element.el
            [ Element.paddingXY 20 0
            ]


viewRecentSessions : List Class.Class -> List ( Class.ClassId, Class.ClassSession ) -> Element Msg
viewRecentSessions classes sessions =
    let
        toHeader =
            Utils.toHeader

        cell =
            Utils.cell HoveredChangedSession Nothing -1
    in
    -- Sessions in a calendar format
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.text "Sessions Attended" |> Element.el [ Font.size 16, Font.bold ]
        , Element.indexedTable
            []
            { columns =
                [ { header = "Date" |> toHeader
                  , width = Element.fill |> Element.maximum 200
                  , view = (\( id, s ) -> Date.toIsoString s.date) >> Element.text >> Element.el [ Element.centerY ] |> cell
                  }
                , { header = "Remarks" |> toHeader
                  , width = Element.fill |> Element.maximum 150
                  , view =
                        (\( id, s ) -> s.remarks)
                            >> Element.text
                            >> Element.el [ Element.centerY ]
                            |> cell
                  }
                , { header = "Duration" |> toHeader
                  , width = Element.fill |> Element.maximum 150
                  , view =
                        (\( id, s ) -> s.duration |> String.fromFloat)
                            >> Element.text
                            >> Element.el [ Element.centerY ]
                            |> cell
                  }
                , { header = "Class" |> toHeader
                  , width = Element.fill |> Element.maximum 80
                  , view =
                        (\( id, s ) -> List.filter (.id >> (==) id) classes |> List.head |> Maybe.map .name |> Maybe.withDefault "")
                            >> Element.text
                            >> Element.el [ Element.centerY ]
                            |> cell
                  }
                ]
            , data = sessions
            }
        ]


viewOtherActivities : List Class.ClassSession -> Element Msg
viewOtherActivities activities =
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 10
        ]
        []


view : Model -> Element Msg
view model =
    let
        sessions =
            model.sessionData |> RemoteData.toMaybe |> Maybe.withDefault Dict.empty

        sessionList : List ( Class.ClassId, ClassSession )
        sessionList =
            List.concatMap
                (\( classId, sessList ) ->
                    List.map (\s -> ( classId, s )) sessList
                )
                (Dict.toList sessions)
                |> List.sortBy (\( id, sess ) -> Date.toRataDie sess.date)
                |> List.reverse

        classList =
            model.classData |> RemoteData.toMaybe |> Maybe.withDefault []
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        , Element.padding 20
        ]
        [ Utils.viewWebData viewDetails model.tutorData
        , Utils.viewWebData viewExtendedDetails model.tutorExtendedData
        , Utils.viewWebData (viewClasses model.hoveredClass (model.tutorHours |> RemoteData.toMaybe |> Maybe.withDefault Dict.empty)) model.classData
        , viewRecentSessions classList sessionList
        , case model.today of
            Just today ->
                viewRecentSessionsCalendar today classList sessionList

            Nothing ->
                Element.none
        , viewOtherActivities []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotToday today ->
            ( { model | today = Just today }, Cmd.none )

        GotTutorData result ->
            ( { model | tutorData = RemoteData.fromResult result }, Cmd.none )

        GotTutorExtendedData result ->
            ( { model | tutorExtendedData = RemoteData.fromResult result }, Cmd.none )

        GotTutorHours result ->
            ( { model | tutorHours = RemoteData.fromResult result }, Cmd.none )

        GotClassData result ->
            ( { model | classData = RemoteData.fromResult result }, Cmd.none )

        GotSessionData result ->
            ( { model | sessionData = RemoteData.fromResult result }, Cmd.none )

        HoveredChangedClass value ->
            ( { model | hoveredClass = value }, Cmd.none )

        HoveredChangedSession value ->
            ( model, Cmd.none )

        ToEditDetails ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "tutor", model.id, "edit" ] []) )

        ToClassDetails classId ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "class", classId ] []) )
