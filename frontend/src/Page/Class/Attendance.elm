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

import Browser.Navigation as Navigation
import Class exposing (ClassSession, ClassTutor)
import Date
import Element exposing (Element)
import Element.Input as Input
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)


type alias Model =
    { key : Navigation.Key
    , classId : Int
    , sessionId : Int
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
    | AddExternalTutor String
    | RemoveExternalTutor String


fetchClassDetails : Model -> Cmd Msg
fetchClassDetails model =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt model.classId
        , expect = Http.expectJson GotClassData Class.classDecoder
        }


fetchSessionDetails : Model -> Cmd Msg
fetchSessionDetails model =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt model.classId ++ "/session/" ++ String.fromInt model.sessionId
        , expect = Http.expectJson GotSessionData Class.classSessionDecoder
        }


fetchTutorsList : Model -> Cmd Msg
fetchTutorsList model =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt model.classId ++ "/session/" ++ String.fromInt model.sessionId ++ "/tutors"
        , expect = Http.expectJson GotTutorsList (Decode.list Class.classTutorDecoder)
        }


fetchPresentList : Model -> Cmd Msg
fetchPresentList model =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt model.classId ++ "/session/" ++ String.fromInt model.sessionId ++ "/attendance"
        , expect = Http.expectJson GotPresentList (Decode.list Decode.string)
        }


postMarkPresent : Int -> Int -> String -> Cmd Msg
postMarkPresent classId sessionId tutorId =
    Http.post
        { url = "http://localhost:5000/class/" ++ String.fromInt classId ++ "/session/" ++ String.fromInt sessionId ++ "/present"
        , body = Http.stringBody "text/plain" tutorId
        , expect = Http.expectWhatever GotMarkedResult
        }


postMarkAbsent : Int -> Int -> String -> Cmd Msg
postMarkAbsent classId sessionId tutorId =
    Http.post
        { url = "http://localhost:5000/class/" ++ String.fromInt classId ++ "/session/" ++ String.fromInt sessionId ++ "/absent"
        , body = Http.stringBody "text/plain" tutorId
        , expect = Http.expectWhatever GotMarkedResult
        }


getNestedNavigation : Model -> List ( String, String )
getNestedNavigation model =
    [ ( "Classes", "/classes" )
    , ( RemoteData.toMaybe model.classData |> Maybe.map .name |> Maybe.withDefault ("Class ID: " ++ String.fromInt model.classId), "/class/" ++ String.fromInt model.classId )
    , ( RemoteData.toMaybe model.sessionData
            |> Maybe.map (.date >> Date.toIsoString)
            |> Maybe.withDefault ("Session ID: " ++ String.fromInt model.sessionId)
      , "/class/"
            ++ String.fromInt model.classId
            ++ "/session/"
            ++ String.fromInt model.sessionId
      )
    ]


getPageTitle : Model -> String
getPageTitle model =
    "Take Attendance"


getPageLink : Model -> String
getPageLink model =
    "/class/"
        ++ String.fromInt model.classId
        ++ "/session/"
        ++ String.fromInt model.sessionId


init : Navigation.Key -> Int -> Int -> ( Model, Cmd Msg )
init key classId sessionId =
    let
        model =
            { key = key
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
        [ fetchPresentList model
        , fetchTutorsList model
        , fetchSessionDetails model
        , fetchClassDetails model
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
            ( model, fetchPresentList model )

        MarkPresent tutorId ->
            case model.present of
                RemoteData.Success presentList ->
                    if List.member tutorId presentList then
                        ignore

                    else
                        ( model, postMarkPresent model.classId model.sessionId tutorId )

                _ ->
                    ignore

        MarkAbsent tutorId ->
            case model.present of
                RemoteData.Success presentList ->
                    if not (List.member tutorId presentList) then
                        ignore

                    else
                        ( model, postMarkAbsent model.classId model.sessionId tutorId )

                _ ->
                    ignore

        AddExternalTutor tutorId ->
            ignore

        RemoveExternalTutor tutorId ->
            ignore


viewSessionInfo : ClassSession -> Element Msg
viewSessionInfo session =
    Element.text (Date.toIsoString session.date)


viewSummary : List ClassTutor -> List String -> Element Msg
viewSummary tutors present =
    Element.text "xxx Tutors Present"


viewAttendance : List ClassTutor -> List String -> Element Msg
viewAttendance tutors present =
    Element.table
        []
        { data = tutors
        , columns =
            [ { header = Element.text "Name"
              , width = Element.fill
              , view = .name >> Element.text
              }
            , { header = Element.none
              , width = Element.fill
              , view =
                    \t ->
                        if List.member t.id present then
                            Element.text "Present"

                        else
                            Element.text "Absent"
              }
            , { header = Element.text "Mark Present"
              , width = Element.fill
              , view = \t -> Input.button [] { label = Element.text "+", onPress = Just (MarkPresent t.id) }
              }
            , { header = Element.text "Mark Absent"
              , width = Element.fill
              , view = \t -> Input.button [] { label = Element.text "-", onPress = Just (MarkAbsent t.id) }
              }
            ]
        }


view : Model -> Element Msg
view model =
    Element.column []
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
                Element.text <| "Failed to get tutor data: " ++ Debug.toString err

            ( _, RemoteData.Failure err ) ->
                Element.text <| "Failed to get attendance data" ++ Debug.toString err

            _ ->
                Element.text "Loading"
        ]
