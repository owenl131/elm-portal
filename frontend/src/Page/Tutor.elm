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
import Class exposing (Class, ClassTutor)
import Colors
import Date
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData, WebData)
import Styles
import Tutor exposing (Tutor, tutorDecoder)
import Url.Builder as Builder
import Utils


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Tutor.TutorId
    , tutorData : WebData Tutor
    , classData : WebData (List Class)
    }


type Msg
    = GotTutorData (Result Http.Error Tutor)
    | GotClassData (Result Http.Error (List Class))
    | ToEditDetails
    | ToClassDetails Class.ClassId


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
      , classData = RemoteData.Loading
      }
    , Cmd.batch [ fetchTutorData credentials id, fetchClassData credentials id ]
    )


fetchTutorData : Api.Credentials -> String -> Cmd Msg
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


fetchClassData : Api.Credentials -> String -> Cmd Msg
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


viewRow : String -> Tutor -> (Tutor -> String) -> Element Msg
viewRow label tutor accessor =
    Element.row
        []
        [ Element.text label |> Element.el [ Element.width (Element.px 150) ]
        , Element.text (accessor tutor) |> Element.el []
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
        , viewRow "Name" data .name
        , viewRow "Email" data .email
        , viewRow "School" data .school
        , viewRow "Status" data (.status >> Tutor.tutorStatusAsString)
        , viewRow "Admin Level" data (.admin >> Tutor.adminLevelAsString)
        , viewRow "Gender" data (.gender >> Tutor.genderToString)
        , viewRow "Date of Birth" data (.dateOfBirth >> Date.toIsoString)
        , viewRow "Start date" data (.dateOfRegistration >> Date.toIsoString)
        ]


viewClasses : List Class.Class -> Element Msg
viewClasses classes =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, bottom = 5, left = 0, right = 3 } ]
    in
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.text "Classes" |> Element.el [ Font.size 16, Font.bold ]
        , Element.table
            [ Element.spacing 5 ]
            { columns =
                [ { header = "Name" |> toHeader
                  , width = Element.fill |> Element.maximum 200
                  , view = .name >> Element.text >> Element.el [ Element.centerY ]
                  }
                , { header = "Days" |> toHeader
                  , width = Element.fill |> Element.maximum 150
                  , view = .days >> List.map Utils.daysToString >> List.intersperse ", " >> String.concat >> Element.text >> Element.el [ Element.centerY ]
                  }
                , { header = "Year" |> toHeader
                  , width = Element.fill |> Element.maximum 80
                  , view = .year >> String.fromInt >> Element.text >> Element.el [ Element.centerY ]
                  }
                , { header = "Details" |> toHeader
                  , width = Element.fill |> Element.maximum 60
                  , view =
                        \class ->
                            Input.button
                                [ Background.color Colors.theme.a400
                                , Border.width 1
                                , Border.rounded 3
                                , Element.paddingXY 10 2
                                , Element.mouseOver [ Background.color Colors.theme.a200 ]
                                ]
                                { label = Element.text "More" |> Element.el [ Element.centerX ]
                                , onPress = Just (ToClassDetails class.id)
                                }
                  }
                ]
            , data = classes
            }
        ]


viewRecentSessions : List Class.ClassSession -> Element Msg
viewRecentSessions sessions =
    -- Sessions in a calendar format
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 10
        ]
        []


viewHours : List Class.Class -> Element Msg
viewHours classStats =
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 10
        ]
        []


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
    Element.column [ Element.width Element.fill, Element.spacing 20 ]
        [ Utils.viewWebData viewDetails model.tutorData
        , Utils.viewWebData viewClasses model.classData
        , viewRecentSessions []
        , viewOtherActivities []
        , viewHours []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTutorData result ->
            ( { model | tutorData = RemoteData.fromResult result }, Cmd.none )

        GotClassData result ->
            ( { model | classData = RemoteData.fromResult result }, Cmd.none )

        ToEditDetails ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "tutor", model.id, "edit" ] []) )

        ToClassDetails classId ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "class", classId ] []) )
