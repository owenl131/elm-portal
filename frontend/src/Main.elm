module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Page.Class as ClassPage
import Page.Class.AddTutor as ClassAddTutorPage
import Page.Class.Attendance as ClassAttendancePage
import Page.ClassList as ClassListPage
import Page.Home as Home
import Page.Login as Login
import Page.Tutor as TutorPage
import Page.TutorList as TutorListPage
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as Query



-- type alias Credentials =
--     { email : String
--     , session : String
--     }


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | NavigateTo String
    | GotLoginMsg Login.Msg
    | GotHomeMsg Home.Msg
    | GotTutorListMsg TutorListPage.Msg
    | GotTutorMsg TutorPage.Msg
    | GotClassListMsg ClassListPage.Msg
    | GotClassMsg ClassPage.Msg
    | GotClassAddTutorMsg ClassAddTutorPage.Msg
    | GotClassAttendanceMsg ClassAttendancePage.Msg


type Model
    = LoggedOut Login.Model
    | Home Home.Model
    | TutorListPage TutorListPage.Model
    | TutorPage TutorPage.Model
    | ClassListPage ClassListPage.Model
    | ClassPage ClassPage.Model
    | ClassAddTutorPage ClassAddTutorPage.Model
    | ClassAttendancePage ClassAttendancePage.Model



-- | AdminPage


type Route
    = RouteHome
    | RouteTutors TutorListPage.TutorFilters (Maybe Int)
    | RouteTutor String
    | RouteClasses ClassListPage.ClassFilters
    | RouteClass Int
    | RouteClassAddTutor Int
    | RouteClassAttendance Int Int
    | NotFound


routeParser : UrlParser.Parser (Route -> Route) Route
routeParser =
    UrlParser.oneOf
        [ UrlParser.map RouteHome (UrlParser.s "home")
        , UrlParser.map RouteTutors
            (UrlParser.s "tutors" <?> TutorListPage.tutorFiltersFromUrl <?> Query.int "page")
        , UrlParser.map RouteTutor (UrlParser.s "tutor" </> UrlParser.string)
        , UrlParser.map RouteClasses
            (UrlParser.s "classes" <?> ClassListPage.classFiltersFromUrl)
        , UrlParser.map RouteClass
            (UrlParser.s "class" </> UrlParser.int)
        , UrlParser.map RouteClassAddTutor
            (UrlParser.s "class" </> UrlParser.int </> UrlParser.s "addtutor")
        , UrlParser.map RouteClassAttendance
            (UrlParser.s "class" </> UrlParser.int </> UrlParser.s "session" </> UrlParser.int)
        ]


getNestedNavigation : Model -> List ( String, String )
getNestedNavigation model =
    case model of
        LoggedOut _ ->
            []

        Home _ ->
            [ ( "Home", "/" ) ]

        TutorListPage _ ->
            [ ( "Tutors", "/tutors" ) ]

        ClassListPage _ ->
            [ ( "Classes", "/classes" ) ]

        TutorPage submodel ->
            [ ( "Tutors", "/tutors" ), ( TutorPage.getPageTitle submodel, TutorPage.getPageLink submodel.id ) ]

        ClassPage submodel ->
            [ ( "Classes", "/classes" ), ( ClassPage.getPageTitle submodel, ClassPage.getPageLink submodel ) ]

        ClassAddTutorPage submodel ->
            ClassAddTutorPage.getNestedNavigation submodel

        ClassAttendancePage submodel ->
            ClassAttendancePage.getNestedNavigation submodel


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

        ClassPage submodel ->
            submodel.key

        ClassAddTutorPage submodel ->
            submodel.key

        ClassAttendancePage submodel ->
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

        RouteTutors filters maybePage ->
            TutorListPage.init key filters (maybePage |> Maybe.withDefault 0)
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

        RouteClass id ->
            ClassPage.init id key
                |> Tuple.mapFirst ClassPage
                |> Tuple.mapSecond (Cmd.map GotClassMsg)

        RouteClassAddTutor id ->
            ClassAddTutorPage.init key id
                |> Tuple.mapFirst ClassAddTutorPage
                |> Tuple.mapSecond (Cmd.map GotClassAddTutorMsg)

        RouteClassAttendance classId sessionId ->
            ClassAttendancePage.init key classId sessionId
                |> Tuple.mapFirst ClassAttendancePage
                |> Tuple.mapSecond (Cmd.map GotClassAttendanceMsg)

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

        GotHomeMsg subMsg ->
            case model of
                Home submodel ->
                    case Home.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( Home newModel, Cmd.map GotHomeMsg newMsg )

                _ ->
                    ignore

        GotLoginMsg subMsg ->
            case model of
                LoggedOut submodel ->
                    case Login.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( LoggedOut newModel, Cmd.map GotLoginMsg newMsg )

                _ ->
                    ignore

        GotTutorListMsg subMsg ->
            case model of
                TutorListPage submodel ->
                    case TutorListPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( TutorListPage newModel, Cmd.map GotTutorListMsg newMsg )

                _ ->
                    ignore

        GotTutorMsg subMsg ->
            case model of
                TutorPage submodel ->
                    case TutorPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( TutorPage newModel, Cmd.map GotTutorMsg newMsg )

                _ ->
                    ignore

        GotClassListMsg subMsg ->
            case model of
                ClassListPage submodel ->
                    case ClassListPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( ClassListPage newModel, Cmd.map GotClassListMsg newMsg )

                _ ->
                    ignore

        GotClassMsg subMsg ->
            case model of
                ClassPage submodel ->
                    case ClassPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( ClassPage newModel, Cmd.map GotClassMsg newMsg )

                _ ->
                    ignore

        GotClassAddTutorMsg subMsg ->
            case model of
                ClassAddTutorPage submodel ->
                    case ClassAddTutorPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( ClassAddTutorPage newModel, Cmd.map GotClassAddTutorMsg newMsg )

                _ ->
                    ignore

        GotClassAttendanceMsg subMsg ->
            case model of
                ClassAttendancePage submodel ->
                    let
                        ( newModel, newMsg ) =
                            ClassAttendancePage.update subMsg submodel
                    in
                    ( ClassAttendancePage newModel, Cmd.map GotClassAttendanceMsg newMsg )

                _ ->
                    ignore


viewDrawerElement : String -> String -> Element Msg
viewDrawerElement label url =
    Input.button
        [ Background.color Colors.theme.p100
        , Element.width Element.fill
        , Element.paddingXY 30 10
        , Element.mouseOver
            [ Border.color Colors.theme.p600
            , Border.innerShadow
                { blur = 2
                , offset = ( 1, 1 )
                , size = 1
                , color = Colors.theme.p800
                }
            ]
        ]
        { onPress = Just (NavigateTo url)
        , label = Element.text label
        }


viewDrawer : Model -> Element Msg
viewDrawer _ =
    Element.column
        [ Element.height Element.fill
        , Element.spacing 3
        , Background.color Colors.theme.p500
        , Font.color Colors.black
        ]
        [ Element.el [ Element.height (Element.px 40) ] Element.none
        , viewDrawerElement "Home" "/home"
        , viewDrawerElement "Tutors" "/tutors"
        , viewDrawerElement "Classes" "/classes"
        , viewDrawerElement "Logout" "/"
        ]


viewTopNavigationElement : ( String, String ) -> Element Msg
viewTopNavigationElement ( label, route ) =
    Input.button
        [ Element.mouseOver [ Border.color Colors.black ]
        , Border.color Colors.clear
        , Border.widthEach { bottom = 1, top = 0, right = 0, left = 0 }
        ]
        { onPress = Just (NavigateTo route), label = Element.text label }


viewTopNavigation : Model -> Element Msg
viewTopNavigation model =
    let
        routes =
            getNestedNavigation model
    in
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px 43)
        , Background.color Colors.theme.p500
        , Element.spacing 20
        , Element.padding 10
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
                , Element.padding 20
                ]
                body
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Upstars Portal"
    , body =
        [ Element.layout
            [ Font.size 14 ]
          <|
            case model of
                LoggedOut submodel ->
                    Element.map GotLoginMsg (Login.view submodel)

                Home submodel ->
                    viewWrapped model <| Element.map GotHomeMsg (Home.view submodel)

                TutorListPage submodel ->
                    viewWrapped model <| Element.map GotTutorListMsg (TutorListPage.view submodel)

                TutorPage submodel ->
                    viewWrapped model <| Element.map GotTutorMsg (TutorPage.view submodel)

                ClassListPage submodel ->
                    viewWrapped model <| Element.map GotClassListMsg (ClassListPage.view submodel)

                ClassPage submodel ->
                    viewWrapped model <| Element.map GotClassMsg (ClassPage.view submodel)

                ClassAddTutorPage submodel ->
                    viewWrapped model <| Element.map GotClassAddTutorMsg (ClassAddTutorPage.view submodel)

                ClassAttendancePage submodel ->
                    viewWrapped model <| Element.map GotClassAttendanceMsg (ClassAttendancePage.view submodel)
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
