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
import Utils


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , classId : Class.ClassId
    , sessionId : Class.SessionId
    , classData : WebData Class.Class
    , sessionData : WebData Class.ClassSession
    , tutors : WebData (List Class.ClassTutor)
    , present : WebData (List Class.ClassId)
    , absent : WebData (List Class.ClassId)
    , hoveredIndex : Int
    , mode : String
    }


type Msg
    = GotTutorsList (Result Http.Error (List Class.ClassTutor))
    | GotPresentList (Result Http.Error (List Class.ClassId))
    | GotAbsentList (Result Http.Error (List Class.ClassId))
    | GotSessionData (Result Http.Error Class.ClassSession)
    | GotClassData (Result Http.Error Class.Class)
    | GotMarkedResult (Result Http.Error ())
    | MarkPresent Class.ClassId
    | MarkAbsent Class.ClassId
    | MarkExempt Class.ClassId
    | HoverChanged Int
    | ModeChanged



-- | AddExternalTutor Class.ClassId
-- | RemoveExternalTutor Class.ClassId


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


fetchAbsentList : Api.Credentials -> Class.ClassId -> Class.SessionId -> Cmd Msg
fetchAbsentList credentials classId sessionId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId, "absent" ] []
        , expect = Http.expectJson GotAbsentList (Decode.list Decode.string)
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


postMarkExempt : Api.Credentials -> Class.ClassId -> Class.SessionId -> Tutor.TutorId -> Cmd Msg
postMarkExempt credentials classId sessionId tutorId =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "session", sessionId, "exempt", tutorId ] []
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
            , absent = RemoteData.Loading
            , tutors = RemoteData.Loading
            , hoveredIndex = -1
            , mode = "view"
            }
    in
    ( model
    , Cmd.batch
        [ fetchPresentList model.credentials model.classId model.sessionId
        , fetchAbsentList model.credentials model.classId model.sessionId
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

        GotAbsentList result ->
            ( { model | absent = RemoteData.fromResult result }, Cmd.none )

        GotSessionData result ->
            ( { model | sessionData = RemoteData.fromResult result }, Cmd.none )

        GotTutorsList result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        GotClassData result ->
            ( { model | classData = RemoteData.fromResult result }, Cmd.none )

        GotMarkedResult _ ->
            ( model
            , Cmd.batch
                [ fetchPresentList model.credentials model.classId model.sessionId
                , fetchAbsentList model.credentials model.classId model.sessionId
                ]
            )

        HoverChanged value ->
            ( { model | hoveredIndex = value }, Cmd.none )

        ModeChanged ->
            ( { model | mode = Utils.ifElse "view" "edit" (model.mode == "edit") }, Cmd.none )

        MarkPresent tutorId ->
            ( model, postMarkPresent model.credentials model.classId model.sessionId tutorId )

        MarkAbsent tutorId ->
            ( model, postMarkAbsent model.credentials model.classId model.sessionId tutorId )

        MarkExempt tutorId ->
            ( model, postMarkExempt model.credentials model.classId model.sessionId tutorId )



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
        [ Element.spacing 8
        , Element.width Element.fill
        , Element.padding 20
        , Background.color Colors.theme.p50
        ]
        [ Element.text "Summary" |> Element.el [ Font.size 16, Font.bold ]
        , Element.text ((List.length present |> String.fromInt) ++ " Tutors Present")
        , Element.text (((List.length tutors - List.length present) |> String.fromInt) ++ " Tutors Absent")
        , Element.text ((List.length tutors |> String.fromInt) ++ " Tutors Total")
        ]


viewAttendance : String -> Int -> List ClassTutor -> List String -> List String -> Element Msg
viewAttendance mode hoveredIndex tutors present absent =
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

        cellCentered =
            Utils.cellCentered HoverChanged Nothing hoveredIndex

        cell : (ClassTutor -> Element Msg) -> Int -> ClassTutor -> Element Msg
        cell =
            Utils.cell HoverChanged Nothing hoveredIndex

        presentButton : ClassTutor -> Element Msg
        presentButton t =
            Input.button
                Styles.buttonStyleMedium
                { label = Element.text "+" |> Element.el [ Element.centerX ]
                , onPress = Just (MarkPresent t.id)
                }

        absentButton : ClassTutor -> Element Msg
        absentButton t =
            Input.button
                Styles.buttonStyleMediumRed
                { label = Element.text "-" |> Element.el [ Element.centerX ]
                , onPress = Just (MarkAbsent t.id)
                }

        exemptButton : ClassTutor -> Element Msg
        exemptButton t =
            Input.button
                Styles.buttonStyleMediumWhite
                { label = Element.text "=" |> Element.el [ Element.centerX ]
                , onPress = Just (MarkExempt t.id)
                }

        getStatus : ClassTutor -> String
        getStatus =
            \t ->
                if List.member t.id present then
                    "Present"

                else if List.member t.id absent then
                    "Absent"

                else
                    "Exempt"
    in
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        [ viewSummary tutors present
        , Element.row
            [ Element.padding 10
            , Element.spacing 5
            ]
            [ Element.text "Mode: "
            , Input.button
                [ Background.color (Utils.ifElse Colors.theme.a400 Colors.white (mode == "view"))
                , Font.color (Utils.ifElse Colors.black Colors.grey (mode == "view"))
                , Element.paddingXY 5 2
                , Border.rounded 3
                , Border.width 1
                ]
                { label = Element.text "View Mode"
                , onPress = Just ModeChanged
                }
            , Input.button
                [ Background.color (Utils.ifElse Colors.theme.a400 Colors.white (mode == "edit"))
                , Font.color (Utils.ifElse Colors.black Colors.grey (mode == "edit"))
                , Element.paddingXY 5 2
                , Border.rounded 3
                , Border.width 1
                ]
                { label = Element.text "Edit Mode"
                , onPress = Just ModeChanged
                }
            ]
        , Utils.ifElse
            (Element.row [ Element.padding 10, Element.spacing 10, Element.width Element.fill ]
                [ Input.button
                    (Element.centerX
                        :: Styles.buttonStyleMedium
                    )
                    { label = "All Present" |> Element.text |> Element.el [ Element.centerX ]
                    , onPress = Nothing
                    }
                , Input.button (Element.centerX :: Styles.buttonStyleMediumRed)
                    { label = "All Absent" |> Element.text |> Element.el [ Element.centerX ]
                    , onPress = Nothing
                    }
                ]
            )
            Element.none
            (mode == "edit")
        , Element.indexedTable
            [ Element.padding 20
            , Element.width Element.fill
            , Border.color Colors.theme.p50
            , Border.width 2
            ]
            { data = tutors
            , columns =
                [ { header = "Name" |> toHeader
                  , width = Element.shrink |> Element.minimum 120
                  , view =
                        (\t ->
                            t.name
                                |> Element.text
                                |> Element.el (Utils.ifElse [ Font.color Colors.grey ] [] (getStatus t == "Exempt"))
                        )
                            |> cell
                  }
                , { header = Element.none
                  , width = Element.shrink |> Element.minimum 100
                  , view =
                        (\t ->
                            getStatus t
                                |> Element.text
                                |> Element.el (Utils.ifElse [ Font.color Colors.grey ] [] (getStatus t == "Exempt"))
                        )
                            |> cell
                  }
                ]
                    ++ Utils.ifElse
                        [ { header = "Mark Present" |> centeredHeader
                          , width = Element.fill |> Element.maximum 100
                          , view = presentButton |> cellCentered
                          }
                        , { header = "Mark Absent" |> centeredHeader
                          , width = Element.fill |> Element.maximum 100
                          , view = absentButton |> cellCentered
                          }
                        , { header = "Mark Exempted" |> centeredHeader
                          , width = Element.fill |> Element.maximum 100
                          , view = exemptButton |> cellCentered
                          }
                        ]
                        []
                        (mode == "edit")
            }
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing 10
        , Element.padding 20
        , Element.width Element.fill
        ]
        [ Utils.viewWebData viewSessionInfo model.sessionData
        , case ( model.tutors, model.present, model.absent ) of
            ( RemoteData.Success tutorList, RemoteData.Success presentList, RemoteData.Success absentList ) ->
                viewAttendance model.mode model.hoveredIndex tutorList presentList absentList

            ( RemoteData.Failure err, _, _ ) ->
                Element.text "Failed to get tutor data"

            ( _, RemoteData.Failure err, _ ) ->
                Element.text "Failed to get attendance data"

            ( _, _, RemoteData.Failure err ) ->
                Element.text "Failed to get attendance data"

            _ ->
                Element.text "Loading"
        ]
