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
import Class exposing (ClassTutor)
import Colors
import Date
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import RemoteData exposing (WebData)
import Styles
import Tutor exposing (Tutor, tutorDecoder)
import Url.Builder as Builder


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : String
    , data : WebData Tutor
    }


type Msg
    = GotTutorData (Result Http.Error Tutor)
    | ToEditDetails


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.data |> Maybe.map .name |> Maybe.withDefault "Tutor"


getPageLink : String -> String
getPageLink id =
    Builder.absolute [ "tutor", id ] []


init : Api.Credentials -> Navigation.Key -> String -> ( Model, Cmd Msg )
init credentials key id =
    ( { key = key, credentials = credentials, id = id, data = RemoteData.Loading }
    , Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id ] []
        , expect = Http.expectJson GotTutorData tutorDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
    )


viewRow : String -> Tutor -> (Tutor -> String) -> Element Msg
viewRow label tutor accessor =
    Element.row
        []
        [ Element.text label |> Element.el [ Element.width (Element.px 150) ]
        , Element.text (accessor tutor) |> Element.el []
        ]


viewWebData : WebData a -> (a -> Element Msg) -> Element Msg
viewWebData webdata viewFn =
    case webdata of
        RemoteData.NotAsked ->
            Element.text "Not Asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure err ->
            Element.text (Api.errorToString err)

        RemoteData.Success data ->
            viewFn data


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
    Element.column
        [ Background.color Colors.theme.p50
        , Element.padding 20
        , Element.width Element.fill
        , Element.spacing 10
        ]
        []


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
        [ viewWebData model.data viewDetails
        , viewClasses []
        , viewRecentSessions []
        , viewOtherActivities []
        , viewHours []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTutorData result ->
            ( { model | data = RemoteData.fromResult result }, Cmd.none )

        ToEditDetails ->
            ( model, Navigation.pushUrl model.key (Builder.absolute [ "tutor", model.id, "edit" ] []) )
