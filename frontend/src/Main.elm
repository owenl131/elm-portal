module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (Element)
import Element.Background as Background
import Element.Input as Input
import Page.Home as Home
import Page.Login as Login
import Page.Tutor as TutorPage
import Page.TutorList as TutorListPage
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((<?>), (</>))
import Url.Parser.Query as Query

type alias Credentials =
    { email : String
    , session : String
    }


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | NavigateTo String
    | GotLoginMsg Login.Msg
    | GotHomeMsg Home.Msg
    | GotTutorListMsg TutorListPage.Msg
    | GotTutorMsg TutorPage.Msg


type Context
    = Context Navigation.Key


type Model
    = LoggedOut Login.Model
    | Home Home.Model
    | TutorListPage TutorListPage.Model
    | TutorPage TutorPage.Model



-- | ClassPage
-- | AttendancePage
-- | AdminPage


type Route
    = RouteHome
    | RouteTutors (Maybe String)
    | RouteTutor String


routeParser =
    UrlParser.oneOf
        [ UrlParser.map RouteHome (UrlParser.s "home")
        , UrlParser.map RouteTutors 
            (UrlParser.s "tutors" 
                <?> Query.string "name") ]


getNavigationKey : Model -> Navigation.Key
getNavigationKey model =
    case model of
        LoggedOut submodel ->
            submodel.key

        Home submodel ->
            submodel.key

        TutorListPage submodel ->
            submodel.key

        TutorPage submodel ->
            submodel.key


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    case url.path of
        "/home" ->
            ( Home.init (getNavigationKey model), Cmd.none )
                |> Tuple.mapFirst Home

        "/tutors" ->
            TutorListPage.init (getNavigationKey model)
                |> Tuple.mapFirst TutorListPage
                |> Tuple.mapSecond (Cmd.map GotTutorListMsg)

        _ ->
            ( Home.init (getNavigationKey model), Cmd.none )
                |> Tuple.mapFirst Home


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( LoggedOut (Login.init key), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )

        _ =
            Debug.log "MSG" (Debug.toString msg)
    in
    case msg of
        -- Handle app-level updates
        ChangeUrl url ->
            handleUrlChange url model

        ClickLink _ ->
            ignore

        NavigateTo urlString ->
            ( model, Navigation.pushUrl (getNavigationKey model) urlString )

        GotHomeMsg homeMsg ->
            case model of
                Home homeModel ->
                    case Home.update homeMsg homeModel of
                        ( newModel, newMsg ) ->
                            ( Home newModel, Cmd.map GotHomeMsg newMsg )

                _ ->
                    ignore

        GotLoginMsg loginMsg ->
            case model of
                LoggedOut loginModel ->
                    case Login.update loginMsg loginModel of
                        ( newModel, newMsg ) ->
                            ( LoggedOut newModel, Cmd.map GotLoginMsg newMsg )

                _ ->
                    ignore

        GotTutorListMsg tutorListMsg ->
            case model of
                TutorListPage tutorListModel ->
                    case TutorListPage.update tutorListMsg tutorListModel of
                        ( newModel, newMsg ) ->
                            ( TutorListPage newModel, Cmd.map GotTutorListMsg newMsg )

                _ ->
                    ignore

        GotTutorMsg tutorMsg ->
            case model of
                TutorPage tutorModel ->
                    case TutorPage.update tutorMsg tutorModel of
                        ( newModel, newMsg ) ->
                            ( TutorPage newModel, Cmd.map GotTutorMsg newMsg )

                _ ->
                    ignore


viewDrawerElement : String -> String -> Element Msg
viewDrawerElement label url =
    Input.button
        []
        { onPress = Just (NavigateTo url)
        , label = Element.text label
        }


viewDrawer : Model -> Element Msg
viewDrawer _ =
    Element.column
        [ Element.height Element.fill
        , Element.padding 40
        , Element.spacing 10
        , Background.color (Element.rgb255 100 100 100)
        ]
        [ viewDrawerElement "Home" "/home"
        , viewDrawerElement "Tutors" "/tutors"
        , viewDrawerElement "Logout" "/"
        ]


viewWrapped : Model -> Element Msg -> Element Msg
viewWrapped model body =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewDrawer model
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 50
            ]
            body
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Upstars Portal"
    , body =
        [ Element.layout
            []
          <|
            case model of
                LoggedOut loginModel ->
                    Element.map GotLoginMsg (Login.view loginModel)

                Home homeModel ->
                    viewWrapped model <| Element.map GotHomeMsg (Home.view homeModel)

                TutorListPage tutorListModel ->
                    viewWrapped model <| Element.map GotTutorListMsg (TutorListPage.view tutorListModel)

                TutorPage tutorModel ->
                    viewWrapped model <| Element.map GotTutorMsg (TutorPage.view tutorModel)

        -- ClassPage ->
        --     Element.none
        -- AttendancePage ->
        --     Element.none
        -- AdminPage ->
        --     Element.none
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = ChangeUrl
        , onUrlRequest = ClickLink
        }
