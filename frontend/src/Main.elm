module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Input as Input
import Page.ClassList as ClassListPage
import Page.Home as Home
import Page.Login as Login
import Page.Tutor as TutorPage
import Page.TutorList as TutorListPage
import Tutor exposing (Tutor)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>))


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
    | GotClassListMsg ClassListPage.Msg


type Model
    = LoggedOut Login.Model
    | Home Home.Model
    | TutorListPage TutorListPage.Model
    | TutorPage TutorPage.Model
    | ClassListPage ClassListPage.Model



-- | ClassPage
-- | AttendancePage
-- | AdminPage


type Route
    = RouteHome
    | RouteTutors TutorListPage.TutorFilters
    | RouteTutor String
    | RouteClasses ClassListPage.ClassFilters
    | NotFound


routeParser : UrlParser.Parser (Route -> Route) Route
routeParser =
    UrlParser.oneOf
        [ UrlParser.map RouteHome (UrlParser.s "home")
        , UrlParser.map RouteTutors
            (UrlParser.s "tutors" <?> TutorListPage.tutorFiltersFromUrl)
        , UrlParser.map RouteTutor (UrlParser.s "tutor" </> UrlParser.string)
        , UrlParser.map RouteClasses
            (UrlParser.s "classes" <?> ClassListPage.classFiltersFromUrl)
        ]


getNestedNavigation : Model -> List ( String, String )
getNestedNavigation model =
    case model of
        LoggedOut _ ->
            []

        Home _ -> 
            [ ("Home", "/")]

        TutorListPage _ ->
            [ ( "Tutors", "/tutors" ) ]

        ClassListPage _ ->
            [ ( "Classes", "/classes" ) ]

        TutorPage submodel ->
            [ ( "Tutors", "/tutors" ), ( TutorPage.getPageTitle submodel, TutorPage.getPageLink submodel ) ]


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

        ClassListPage submodel ->
            submodel.key


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        key =
            getNavigationKey model
    in
    case Maybe.withDefault NotFound (UrlParser.parse routeParser url) of
        RouteHome ->
            ( Home.init key, Cmd.none )
                |> Tuple.mapFirst Home

        RouteTutors filters ->
            TutorListPage.init key filters
                |> Tuple.mapFirst TutorListPage
                |> Tuple.mapSecond (Cmd.map GotTutorListMsg)

        RouteTutor id ->
            TutorPage.init key id
                |> Tuple.mapFirst TutorPage
                |> Tuple.mapSecond (Cmd.map GotTutorMsg)

        RouteClasses filters ->
            ClassListPage.init key filters
                |> Tuple.mapFirst ClassListPage
                |> Tuple.mapSecond (Cmd.map GotClassListMsg)

        NotFound ->
            ( Home.init key, Cmd.none )
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

        GotClassListMsg classListMsg ->
            case model of
                ClassListPage classListModel ->
                    case ClassListPage.update classListMsg classListModel of
                        ( newModel, newMsg ) ->
                            ( ClassListPage newModel, Cmd.map GotClassListMsg newMsg )

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
        , viewDrawerElement "Classes" "/classes"
        , viewDrawerElement "Logout" "/"
        ]


viewTopNavigationElement : ( String, String ) -> Element Msg
viewTopNavigationElement ( label, route ) =
    Input.button [] { onPress = Just (NavigateTo route), label = Element.text label }


viewTopNavigation : Model -> Element Msg
viewTopNavigation model =
    let
        routes =
            getNestedNavigation model
    in
    Element.row
        [ Element.width Element.fill
        , Background.color (Element.rgb255 100 100 100)
        ]
        (List.map viewTopNavigationElement routes |> List.intersperse (Element.text ">"))


viewWrapped : Model -> Element Msg -> Element Msg
viewWrapped model body =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewDrawer model
        , Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ viewTopNavigation model
            , Element.el 
                [ Element.width Element.fill 
                , Element.height Element.fill 
                , Element.padding 50 ]
                body
            ]
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

                ClassListPage classListModel ->
                    viewWrapped model <| Element.map GotClassListMsg (ClassListPage.view classListModel)

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
