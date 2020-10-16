module Page.Class exposing (Model, Msg, getPageLink, getPageTitle, init, update, view)

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
import Http
import Json.Decode as Decode
import Page.Tutor
import RemoteData exposing (WebData)
import Styles
import Url.Builder as Builder


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : ClassId
    , data : WebData Class
    , sessions : WebData (List ClassSession)
    , tutors : WebData (List ClassTutor)
    }


type Msg
    = GotClassData (Result Http.Error Class)
    | GotSessionsData (Result Http.Error (List ClassSession))
    | GotTutorsData (Result Http.Error (List ClassTutor))
    | NavigateToTutor String
    | NavigateToAddTutors
    | NavigateToTakeAttendance Int
    | DisplayAddSession


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
            }
    in
    ( model
    , Cmd.batch
        [ fetchClassData model.credentials model.id
        , fetchSessionsData model.credentials model.id
        , fetchTutorData model.credentials model.id
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotClassData result ->
            ( { model | data = RemoteData.fromResult result }, Cmd.none )

        GotSessionsData result ->
            ( { model | sessions = RemoteData.fromResult result }, Cmd.none )

        GotTutorsData result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        NavigateToTutor id ->
            ( model, Navigation.pushUrl model.key (Page.Tutor.getPageLink id) )

        NavigateToAddTutors ->
            ( model, Navigation.pushUrl model.key (getPageLink model.id ++ "/addtutor") )

        NavigateToTakeAttendance sessionId ->
            ( model, Navigation.pushUrl model.key (getPageLink model.id ++ "/session/" ++ String.fromInt sessionId) )

        DisplayAddSession ->
            ( model, Cmd.none )


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
        [ viewRow "Name" class .name
        , viewRow "Timeslot" class .timeslot
        , viewRow "Duration" class (.duration >> String.fromFloat)
        ]


viewSessions : List ClassSession -> Element Msg
viewSessions sessions =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text
                |> Element.text
                |> Element.el
                    [ Font.bold, Element.paddingEach { top = 0, bottom = 5, left = 0, right = 3 } ]
    in
    Element.column
        [ Element.padding 20
        , Element.spacing 20
        , Element.width Element.fill
        , Background.color Colors.theme.p50
        ]
        [ Input.button
            [ Background.color Colors.theme.a400
            , Border.width 1
            , Border.rounded 3
            , Element.paddingXY 10 4
            , Element.mouseOver [ Background.color Colors.theme.a200 ]
            ]
            { onPress = Just DisplayAddSession, label = Element.text "Add New Session" }
        , Element.table
            [ Element.spacing 5
            , Element.width Element.fill
            ]
            { data = sessions
            , columns =
                [ { header = "Date" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = .date >> Date.toIsoString >> Element.text
                  }
                , { header = "Duration" |> toHeader
                  , width = Element.fill |> Element.maximum 100
                  , view = .duration >> String.fromFloat >> Element.text
                  }
                , { header = "Remarks" |> toHeader
                  , width = Element.fill |> Element.maximum 300
                  , view = .remarks >> Element.text
                  }
                , { header = "Attendance" |> toHeader
                  , width = Element.fill |> Element.maximum 50
                  , view =
                        \sess ->
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
                  }
                ]
            }
        ]


viewTutors : List ClassTutor -> Element Msg
viewTutors tutors =
    Element.column
        [ Element.padding 20
        , Element.spacing 20
        , Element.width Element.fill
        , Background.color Colors.theme.p50
        ]
        [ Input.button
            Styles.buttonStyleComfy
            { onPress = Just NavigateToAddTutors, label = Element.text "Add New Tutor" }
        , let
            toHeader : String -> Element Msg
            toHeader text =
                text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, bottom = 5, left = 0, right = 3 } ]
          in
          Element.table
            [ Element.spacing 5
            ]
            { data = tutors
            , columns =
                [ { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 200
                  , view = .name >> Element.text
                  }
                , { header = "Join Class Date" |> toHeader
                  , width = Element.fill |> Element.maximum 140
                  , view = .joinDate >> Date.toIsoString >> Element.text
                  }
                , { header = "Details" |> toHeader
                  , width = Element.fill |> Element.maximum 60
                  , view =
                        \t ->
                            Input.button
                                [ Background.color Colors.theme.a400
                                , Border.width 1
                                , Border.rounded 3
                                , Element.paddingXY 10 2
                                , Element.mouseOver [ Background.color Colors.theme.a200 ]
                                ]
                                { onPress = Just (NavigateToTutor t.id)
                                , label = Element.text "More" |> Element.el [ Element.centerX ]
                                }
                  }
                ]
            }
        ]


view : Model -> Element Msg
view model =
    -- Should show details, menu to make new and sessions
    -- click session to redirect to take/view attendance page
    -- Display tutor list and button to open add tutor menu
    Element.column
        [ Element.spacing 10
        , Element.height Element.fill
        , Element.width Element.fill
        ]
        [ case model.data of
            RemoteData.Success class ->
                viewDetails class

            RemoteData.Loading ->
                Element.text "Loading"

            RemoteData.NotAsked ->
                Element.text "Not asked"

            RemoteData.Failure err ->
                Element.text (Api.errorToString err)
        , case model.sessions of
            RemoteData.Success sessions ->
                viewSessions sessions

            RemoteData.Loading ->
                Element.text "Loading"

            RemoteData.NotAsked ->
                Element.text "Not asked"

            RemoteData.Failure err ->
                Element.text (Api.errorToString err)
        , case model.tutors of
            RemoteData.Success tutors ->
                viewTutors tutors

            RemoteData.Loading ->
                Element.text "Loading"

            RemoteData.NotAsked ->
                Element.text "Not asked"

            RemoteData.Failure err ->
                Element.text (Api.errorToString err)
        ]
