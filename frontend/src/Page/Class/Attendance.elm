module Page.Class.Attendance exposing
    ( Model
    , Msg
    , getNestedNavigation
    , getPageLink
    , getPageTitle
    , init
    , update
    , view
    )

import Api
import Base64
import Browser.Navigation as Navigation
import Class exposing (ClassId, ClassSession, ClassTutor)
import Colors
import Date
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import String
import Styles
import Tutor
import Url.Builder as Builder


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , classId : Class.ClassId
    , sessionId : Class.SessionId
    , classData : WebData Class.Class
    , sessionData : WebData Class.ClassSession
    , tutors : WebData (List Class.ClassTutor)
    , present : WebData (List String)
    , hoveredIndex : Int
    }


type Msg
    = GotTutorsList (Result Http.Error (List Class.ClassTutor))
    | GotPresentList (Result Http.Error (List String))
    | GotSessionData (Result Http.Error Class.ClassSession)
    | GotClassData (Result Http.Error Class.Class)
    | GotMarkedResult (Result Http.Error ())
    | MarkPresent String
    | MarkAbsent String
    | HoverChanged Int



-- | AddExternalTutor String
-- | RemoveExternalTutor String


fetchClassDetails : Api.Credentials -> Class.ClassId -> Cmd Msg
fetchClassDetails credentials classId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId ] []
        , expect = Http.expectJson GotClassData Class.classDecoder
        }


fetchSessionDetails : Api.Credentials -> Class.ClassId -> Class.SessionId -> Cmd Msg
fetchSessionDetails credentials classId sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId ] []
        , expect = Http.expectJson GotSessionData Class.classSessionDecoder
        }


fetchTutorsList : Api.Credentials -> Class.ClassId -> Class.SessionId -> Cmd Msg
fetchTutorsList credentials classId sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId, "tutors" ] []
        , expect = Http.expectJson GotTutorsList (Decode.list Class.classTutorDecoder)
        }


fetchPresentList : Api.Credentials -> Class.ClassId -> Class.SessionId -> Cmd Msg
fetchPresentList credentials classId sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId, "present" ] []
        , expect = Http.expectJson GotPresentList (Decode.list Decode.string)
        }


postMarkPresent : Api.Credentials -> Class.ClassId -> Class.SessionId -> Tutor.TutorId -> Cmd Msg
postMarkPresent credentials classId sessionId tutorId =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId, "present", tutorId ] []
        , expect = Http.expectWhatever GotMarkedResult
        }


postMarkAbsent : Api.Credentials -> Class.ClassId -> Class.SessionId -> Tutor.TutorId -> Cmd Msg
postMarkAbsent credentials classId sessionId tutorId =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId, "absent", tutorId ] []
        , expect = Http.expectWhatever GotMarkedResult
        }


getNestedNavigation : Model -> List ( String, String )
getNestedNavigation model =
    [ ( "Classes", Builder.absolute [ "classes" ] [] )
    , ( RemoteData.toMaybe model.classData
            |> Maybe.map .name
            |> Maybe.withDefault ("Class ID: " ++ model.classId)
      , Builder.absolute [ "class", model.classId ] []
      )
    , ( RemoteData.toMaybe model.sessionData
            |> Maybe.map (.date >> Date.toIsoString)
            |> Maybe.withDefault ("Session ID: " ++ model.sessionId)
      , Builder.absolute [ "class", model.classId, "session", model.sessionId ] []
      )
    ]


getPageTitle : Model -> String
getPageTitle _ =
    "Take Attendance"


getPageLink : Model -> String
getPageLink model =
    Builder.absolute [ "class", model.classId, "session", model.sessionId ] []


init : Api.Credentials -> Navigation.Key -> Class.ClassId -> Class.SessionId -> ( Model, Cmd Msg )
init credentials key classId sessionId =
    let
        model =
            { key = key
            , credentials = credentials
            , classId = classId
            , sessionId = sessionId
            , sessionData = RemoteData.Loading
            , classData = RemoteData.Loading
            , present = RemoteData.Loading
            , tutors = RemoteData.Loading
            , hoveredIndex = -1
            }
    in
    ( model
    , Cmd.batch
        [ fetchPresentList model.credentials model.classId model.sessionId
        , fetchTutorsList model.credentials model.classId model.sessionId
        , fetchSessionDetails model.credentials model.classId model.sessionId
        , fetchClassDetails model.credentials model.classId
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        GotPresentList result ->
            ( { model | present = RemoteData.fromResult result }, Cmd.none )

        GotSessionData result ->
            ( { model | sessionData = RemoteData.fromResult result }, Cmd.none )

        GotTutorsList result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        GotClassData result ->
            ( { model | classData = RemoteData.fromResult result }, Cmd.none )

        GotMarkedResult _ ->
            ( model, fetchPresentList model.credentials model.classId model.sessionId )

        HoverChanged value ->
            ( { model | hoveredIndex = value }, Cmd.none )

        MarkPresent tutorId ->
            case model.present of
                RemoteData.Success presentList ->
                    if List.member tutorId presentList then
                        ignore

                    else
                        ( model, postMarkPresent model.credentials model.classId model.sessionId tutorId )

                _ ->
                    ignore

        MarkAbsent tutorId ->
            case model.present of
                RemoteData.Success presentList ->
                    if not (List.member tutorId presentList) then
                        ignore

                    else
                        ( model, postMarkAbsent model.credentials model.classId model.sessionId tutorId )

                _ ->
                    ignore



-- AddExternalTutor _ ->
--     ignore
-- RemoveExternalTutor _ ->
--     ignore


viewSessionInfo : ClassSession -> Element Msg
viewSessionInfo session =
    Element.column
        [ Element.width Element.fill
        , Background.color Colors.theme.p50
        , Element.padding 20
        , Element.spacing 5
        ]
        [ Element.text
            (Date.toIsoString session.date)

        -- should allow user to edit the duration and remarks
        , Element.text (String.fromFloat session.duration ++ " hours")
        ]


viewSummary : List ClassTutor -> List String -> Element Msg
viewSummary tutors present =
    Element.column
        [ Element.spacing 5
        , Element.width Element.fill
        , Element.padding 20
        , Background.color Colors.theme.p50
        ]
        [ Element.text ((List.length present |> String.fromInt) ++ " Tutors Present")
        , Element.text (((List.length tutors - List.length present) |> String.fromInt) ++ " Tutors Absent")
        , Element.text ((List.length tutors |> String.fromInt) ++ " Tutors Total")
        ]


viewAttendance : Int -> List ClassTutor -> List String -> Element Msg
viewAttendance hoveredIndex tutors present =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text
                |> Element.text
                |> Element.el [ Font.bold, Element.padding 4 ]
                |> Element.el [ Element.paddingXY 0 10 ]

        centeredHeader : String -> Element Msg
        centeredHeader text =
            text
                |> Element.text
                |> Element.el [ Element.centerX, Font.bold, Element.padding 4 ]
                |> Element.el [ Element.width Element.fill, Element.paddingXY 0 10 ]

        cell : (ClassTutor -> Element Msg) -> Int -> ClassTutor -> Element Msg
        cell toElem index e =
            Element.el
                ([ Element.centerY
                 , Events.onMouseEnter (HoverChanged index)
                 , Events.onMouseLeave (HoverChanged -1)
                 , Element.height Element.fill
                 , Element.padding 4
                 ]
                    ++ (if index == hoveredIndex then
                            [ Background.color Colors.theme.p100 ]

                        else
                            []
                       )
                )
                (toElem e |> Element.el [ Element.centerY ])

        presentButton : ClassTutor -> Element Msg
        presentButton t =
            Input.button
                [ Background.color Colors.theme.a400
                , Border.width 1
                , Border.rounded 20
                , Element.paddingXY 10 6
                , Element.width (Element.shrink |> Element.minimum 100)
                , Element.mouseOver [ Background.color Colors.theme.a700 ]
                ]
                { label = Element.text "+" |> Element.el [ Element.centerX ]
                , onPress = Just (MarkPresent t.id)
                }

        absentButton : ClassTutor -> Element Msg
        absentButton t =
            Input.button
                [ Background.color Colors.red
                , Border.width 1
                , Border.rounded 20
                , Element.paddingXY 10 6
                , Element.width (Element.shrink |> Element.minimum 100)
                , Font.color Colors.white
                ]
                { label = Element.text "-" |> Element.el [ Element.centerX ]
                , onPress = Just (MarkAbsent t.id)
                }
    in
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        [ viewSummary tutors present
        , Element.indexedTable
            [ Element.padding 20
            , Element.width Element.fill
            , Border.color Colors.theme.p50
            , Border.width 2
            ]
            { data = tutors
            , columns =
                [ { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = .name >> Element.text |> cell
                  }
                , { header = Element.none
                  , width = Element.fill |> Element.maximum 80
                  , view =
                        (\t ->
                            if List.member t.id present then
                                Element.text "Present"

                            else
                                Element.text "Absent"
                        )
                            |> cell
                  }
                , { header = "Mark Present" |> centeredHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = presentButton |> cell
                  }
                , { header = "Mark Absent" |> centeredHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = absentButton |> cell
                  }
                ]
            }
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing 10
        , Element.padding 20
        , Element.width Element.fill
        ]
        [ case model.sessionData of
            RemoteData.NotAsked ->
                Element.text "Not asked"

            RemoteData.Loading ->
                Element.text "Loading"

            RemoteData.Success data ->
                viewSessionInfo data

            RemoteData.Failure err ->
                Element.text (Debug.toString err)
        , case ( model.tutors, model.present ) of
            ( RemoteData.Success tutorList, RemoteData.Success presentList ) ->
                viewAttendance model.hoveredIndex tutorList presentList

            ( RemoteData.Failure err, _ ) ->
                Element.text ("Failed to get tutor data: " ++ Debug.toString err)

            ( _, RemoteData.Failure err ) ->
                Element.text ("Failed to get attendance data" ++ Debug.toString err)

            _ ->
                Element.text "Loading"
        ]
