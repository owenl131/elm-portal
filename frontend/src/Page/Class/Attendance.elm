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
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import String
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
    }


type Msg
    = GotTutorsList (Result Http.Error (List Class.ClassTutor))
    | GotPresentList (Result Http.Error (List String))
    | GotSessionData (Result Http.Error Class.ClassSession)
    | GotClassData (Result Http.Error Class.Class)
    | GotMarkedResult (Result Http.Error ())
    | MarkPresent String
    | MarkAbsent String



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
        ]
        [ Element.text
            (Date.toIsoString session.date)
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


viewAttendance : List ClassTutor -> List String -> Element Msg
viewAttendance tutors present =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, bottom = 5, left = 0, right = 3 } ]
    in
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        [ viewSummary tutors present
        , Element.table
            [ Element.spacing 5
            , Element.padding 20
            , Element.width Element.fill
            , Border.color Colors.theme.p50
            , Border.width 2
            ]
            { data = tutors
            , columns =
                [ { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = .name >> Element.text
                  }
                , { header = Element.none
                  , width = Element.fill |> Element.maximum 80
                  , view =
                        \t ->
                            if List.member t.id present then
                                Element.text "Present"

                            else
                                Element.text "Absent"
                  }
                , { header = "Mark Present" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = \t -> Input.button [] { label = Element.text "+", onPress = Just (MarkPresent t.id) }
                  }
                , { header = "Mark Absent" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = \t -> Input.button [] { label = Element.text "-", onPress = Just (MarkAbsent t.id) }
                  }
                ]
            }
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing 10
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
                viewAttendance tutorList presentList

            ( RemoteData.Failure err, _ ) ->
                Element.text ("Failed to get tutor data: " ++ Debug.toString err)

            ( _, RemoteData.Failure err ) ->
                Element.text ("Failed to get attendance data" ++ Debug.toString err)

            _ ->
                Element.text "Loading"
        ]
