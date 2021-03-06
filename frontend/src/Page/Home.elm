module Page.Home exposing (Model, Msg, init, update, view)

import Api
import Base64
import Browser.Navigation as Navigation
import Class exposing (Class)
import Colors
import Date
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import Set exposing (Set)
import Styles
import Task
import Tutor exposing (Tutor)
import Url.Builder as Builder
import Utils


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , today : Maybe Date.Date
    , myData : WebData Tutor
    , classesToday : WebData (List Class)
    , myClasses : WebData (List Class)
    , hovered : Set Class.ClassId
    }


type Msg
    = GotClassData (Result Http.Error (List Class))
    | GotMyData (Result Http.Error Tutor)
    | SetToday Date.Date
    | ToClass Class.ClassId
    | ToProfile Tutor.TutorId
    | StartHover Class.ClassId
    | StopHover Class.ClassId


fetchMyClasses : Api.Credentials -> Cmd Msg
fetchMyClasses credentials =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "my", "classes" ] []
        , expect = Http.expectJson GotClassData (Decode.list Class.classDecoder)
        }


fetchMyData : Api.Credentials -> Cmd Msg
fetchMyData credentials =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "my", "details" ] []
        , expect = Http.expectJson GotMyData Tutor.tutorDecoder
        }


init : Api.Credentials -> Navigation.Key -> ( Model, Cmd Msg )
init credentials key =
    ( { key = key
      , credentials = credentials
      , classesToday = RemoteData.NotAsked
      , today = Nothing
      , myClasses = RemoteData.Loading
      , myData = RemoteData.NotAsked
      , hovered = Set.empty
      }
    , Cmd.batch
        [ fetchMyClasses credentials
        , fetchMyData credentials
        , Task.perform SetToday Date.today
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotClassData result ->
            ( { model | myClasses = RemoteData.fromResult result }, Cmd.none )

        GotMyData result ->
            ( { model | myData = RemoteData.fromResult result }, Cmd.none )

        SetToday today ->
            ( { model | today = Just today }, Cmd.none )

        StartHover id ->
            ( { model | hovered = Set.insert id model.hovered }, Cmd.none )

        StopHover id ->
            ( { model | hovered = Set.remove id model.hovered }, Cmd.none )

        ToClass classId ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "class", classId ] []) )

        ToProfile tutorId ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "tutor", tutorId ] []) )


viewMyClassesSingle : Set Class.ClassId -> Maybe Date.Date -> Class -> Element Msg
viewMyClassesSingle hovered today class =
    let
        isHovered =
            Set.member class.id hovered
    in
    Element.el
        [ Element.padding 25
        , Border.color Colors.theme.p400
        , Border.width 1
        , Border.rounded 10
        , Background.color (Utils.ifElse Colors.theme.p100 Colors.clear isHovered)
        , Element.Events.onClick (ToClass class.id)
        , Element.Events.onMouseEnter (StartHover class.id)
        , Element.Events.onMouseLeave (StopHover class.id)
        ]
        (Element.column
            [ Element.spacing 10 ]
            [ Element.text class.name |> Element.el [ Font.size 16, Font.bold ]
            , Element.text (class.year |> String.fromInt)
            , Element.text class.timeslot
            , Element.text (class.days |> List.map Utils.daysToString |> String.join ", ")
            , Element.el
                [ Element.width (Element.px 200)
                , Element.height (Element.px 50)
                ]
                Element.none
            , Element.row [ Element.width Element.fill ]
                [ case today of
                    Nothing ->
                        Element.none

                    Just date ->
                        if List.member (Date.weekday date) class.days then
                            Element.text "Class today!" |> Element.el [ Element.alignLeft ]

                        else
                            Element.none
                , Input.button (Element.alignRight :: Styles.buttonStyleCozy) { onPress = Just (ToClass class.id), label = Element.text "More" }
                ]
            ]
        )


viewMyClasses : Set Class.ClassId -> Maybe Date.Date -> List Class -> Element Msg
viewMyClasses hovered today classes =
    Element.wrappedRow
        [ Element.spacing 20, Element.padding 20 ]
        (List.map (viewMyClassesSingle hovered today) classes)


viewQuickLinks : Tutor -> Element Msg
viewQuickLinks tutor =
    Element.wrappedRow
        [ Element.spacing 20, Element.padding 20 ]
        [ Input.button
            [ Element.paddingXY 40 20
            , Background.color Colors.theme.p200
            , Border.width 1
            , Border.rounded 3
            , Element.mouseOver [ Background.color Colors.theme.a400 ]
            ]
            { label = Element.text "View My Profile →"
            , onPress = Just (ToProfile tutor.id)
            }
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing 5
        , Element.padding 20
        ]
        ([ Utils.viewWebData viewQuickLinks model.myData
         , Utils.viewWebData (viewMyClasses model.hovered model.today) model.myClasses
         , Element.el [ Element.height (Element.px 50) ] Element.none
         ]
            ++ List.map Element.text
                [ "View fields by selection across tutors"
                , "Edit for extended fields"
                , "Calculate level from school type and graduation year"
                , "On tutor page display sessions attended on calendar"
                , "When creating session check for correct day, and check if same session already exists"
                , "View tutor attendance stats in tutor/class"
                , "Mark all present/absent"
                , "Edit session fields from attendance page"
                , "Implement sort by which field"
                , "Implement student fields"
                , "Implement export/import data"
                , "Tag day as attendance unmarked if class is active"
                , "Separate API out into its own file"
                , "Possibly set up a mock API for demo without server"
                , "Add information tooltips around"
                ]
        )
