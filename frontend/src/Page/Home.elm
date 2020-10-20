module Page.Home exposing (Model, Msg, init, update, view)

import Api
import Base64
import Browser.Navigation as Navigation
import Class exposing (Class)
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
    }


type Msg
    = GotClassData (Result Http.Error (List Class))
    | SetToday Date.Date
    | ToClass Class.ClassId


fetchMyClasses : Api.Credentials -> Cmd Msg
fetchMyClasses credentials =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "my", "classes" ] []
        , expect = Http.expectJson GotClassData (Decode.list Class.classDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


init : Api.Credentials -> Navigation.Key -> ( Model, Cmd Msg )
init credentials key =
    ( { key = key
      , credentials = credentials
      , classesToday = RemoteData.NotAsked
      , today = Nothing
      , myClasses = RemoteData.Loading
      , myData = RemoteData.NotAsked
      }
    , Cmd.batch [ fetchMyClasses credentials, Task.perform SetToday Date.today ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotClassData result ->
            ( { model | myClasses = RemoteData.fromResult result }, Cmd.none )

        SetToday today ->
            ( { model | today = Just today }, Cmd.none )

        ToClass classId ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "class", classId ] []) )


viewMyClassesSingle : Maybe Date.Date -> Class -> Element Msg
viewMyClassesSingle today class =
    Element.el
        [ Element.padding 25
        , Border.color Colors.theme.p400
        , Border.width 1
        , Border.rounded 10
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


viewMyClasses : Maybe Date.Date -> List Class -> Element Msg
viewMyClasses today classes =
    Element.wrappedRow
        [ Element.spacing 20, Element.padding 20 ]
        (List.map (viewMyClassesSingle today) classes)


view : Model -> Element Msg
view model =
    Element.column
        [ Element.spacing 5 ]
        ([ Utils.viewWebData (viewMyClasses model.today) model.myClasses
         , Element.el [ Element.height (Element.px 50) ] Element.none
         ]
            ++ List.map Element.text
                [ "Home - Use this for short term dev goals"
                , "Tutor extended fields, view fields by selection across tutors"
                , "Extended fields : languages spoken, days available, subjects keen, career goal, date of graduation, school type, remarks"
                , "Update class fields"
                , "Create class session"
                , "Implement CIP hours page"
                , "Implement sort by which field"
                , "Separate API out into its own file"
                , "Possibly set up a mock API for demo without server"
                ]
        )
