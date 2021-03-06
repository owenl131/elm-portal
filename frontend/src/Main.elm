module Main exposing (main)

import Api
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Class
import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Page.Class as ClassPage
import Page.Class.Attendance as ClassAttendancePage
import Page.Class.Edit as ClassEditPage
import Page.Class.ManageTutors as ClassManageTutorsPage
import Page.ClassList as ClassListPage
import Page.Home as Home
import Page.Login as Login
import Page.Student as StudentPage
import Page.Student.Edit as StudentEditPage
import Page.StudentList as StudentListPage
import Page.Tutor as TutorPage
import Page.Tutor.Edit as TutorEditPage
import Page.TutorList as TutorListPage
import Student exposing (StudentId)
import Tutor exposing (Tutor, TutorId)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>))
import Url.Parser.Query as Query


type Msg
    = ChangeUrl Url
    | ClickLink UrlRequest
    | NavigateTo String
    | GotLoginMsg Login.Msg
    | GotHomeMsg Home.Msg
    | GotTutorListMsg TutorListPage.Msg
    | GotTutorMsg TutorPage.Msg
    | GotTutorEditMsg TutorEditPage.Msg
    | GotStudentListMsg StudentListPage.Msg
    | GotStudentMsg StudentPage.Msg
    | GotStudentEditMsg StudentEditPage.Msg
    | GotClassListMsg ClassListPage.Msg
    | GotClassMsg ClassPage.Msg
    | GotClassManageTutorMsg ClassManageTutorsPage.Msg
    | GotClassAttendanceMsg ClassAttendancePage.Msg
    | GotClassEditMsg ClassEditPage.Msg


type Model
    = LoggedOut Login.Model
    | Home Home.Model
    | TutorListPage TutorListPage.Model
    | TutorPage TutorPage.Model
    | TutorEditPage TutorEditPage.Model
    | StudentListPage StudentListPage.Model
    | StudentPage StudentPage.Model
    | StudentEditPage StudentEditPage.Model
    | ClassListPage ClassListPage.Model
    | ClassPage ClassPage.Model
    | ClassManageTutorsPage ClassManageTutorsPage.Model
    | ClassAttendancePage ClassAttendancePage.Model
    | ClassEditPage ClassEditPage.Model


type Route
    = RouteHome
      -- Tutor Pages
    | RouteTutors TutorListPage.TutorFilters (Maybe Int)
    | RouteTutorNew
    | RouteTutor TutorId
    | RouteTutorEdit TutorId
      -- Student Pages
    | RouteStudents StudentListPage.StudentFilters (Maybe Int)
    | RouteStudentNew
    | RouteStudent StudentId
    | RouteStudentEdit StudentId
      -- Class Pages
    | RouteClasses ClassListPage.ClassFilters (Maybe Int)
    | RouteClassNew
    | RouteClass Class.ClassId
    | RouteClassManageTutors Class.ClassId
    | RouteClassAttendance Class.ClassId Class.SessionId
    | RouteClassEdit Class.ClassId
      -- Others
    | RouteLogout
    | NotFound


routeParser : UrlParser.Parser (Route -> Route) Route
routeParser =
    UrlParser.oneOf
        [ UrlParser.map RouteHome (UrlParser.s "home")

        -- Tutor Pages
        , UrlParser.map RouteTutors
            (UrlParser.s "tutors" <?> TutorListPage.tutorFiltersFromUrl <?> Query.int "page")
        , UrlParser.map RouteTutorNew
            (UrlParser.s "tutors" </> UrlParser.s "new")
        , UrlParser.map RouteTutor (UrlParser.s "tutor" </> UrlParser.string)
        , UrlParser.map RouteTutorEdit (UrlParser.s "tutor" </> UrlParser.string </> UrlParser.s "edit")

        -- Student Pages
        , UrlParser.map RouteStudents
            (UrlParser.s "students" <?> StudentListPage.studentFiltersFromUrl <?> Query.int "page")
        , UrlParser.map RouteStudentNew
            (UrlParser.s "students" </> UrlParser.s "new")
        , UrlParser.map RouteStudent (UrlParser.s "student" </> UrlParser.string)
        , UrlParser.map RouteStudentEdit (UrlParser.s "student" </> UrlParser.string </> UrlParser.s "edit")

        -- Class Pages
        , UrlParser.map RouteClasses
            (UrlParser.s "classes" <?> ClassListPage.classFiltersFromUrl <?> Query.int "page")
        , UrlParser.map RouteClassNew (UrlParser.s "classes" </> UrlParser.s "new")
        , UrlParser.map RouteClass
            (UrlParser.s "class" </> UrlParser.string)
        , UrlParser.map RouteClassManageTutors
            (UrlParser.s "class" </> UrlParser.string </> UrlParser.s "managetutors")
        , UrlParser.map RouteClassAttendance
            (UrlParser.s "class" </> UrlParser.string </> UrlParser.s "session" </> UrlParser.string)
        , UrlParser.map RouteClassEdit (UrlParser.s "class" </> UrlParser.string </> UrlParser.s "edit")

        -- Other Pages
        , UrlParser.map RouteLogout (UrlParser.s "logout")
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

        TutorPage submodel ->
            [ ( "Tutors", "/tutors" ), ( TutorPage.getPageTitle submodel, TutorPage.getPageLink submodel.id ) ]

        TutorEditPage submodel ->
            case submodel.id of
                Nothing ->
                    [ ( TutorEditPage.getPageTitle submodel, TutorEditPage.getPageLink submodel ) ]

                Just id ->
                    [ ( "Tutor Profile", TutorPage.getPageLink id )
                    , ( TutorEditPage.getPageTitle submodel, TutorEditPage.getPageLink submodel )
                    ]

        StudentListPage _ ->
            [ ( "Students", "/students" ) ]

        StudentPage submodel ->
            [ ( "Students", "/students" ), ( StudentPage.getPageTitle submodel, StudentPage.getPageLink submodel.id ) ]

        StudentEditPage submodel ->
            case submodel.id of
                Nothing ->
                    [ ( StudentEditPage.getPageTitle submodel, StudentEditPage.getPageLink submodel ) ]

                Just id ->
                    [ ( "Student Profile", StudentPage.getPageLink id )
                    , ( StudentEditPage.getPageTitle submodel, StudentEditPage.getPageLink submodel )
                    ]

        ClassListPage _ ->
            [ ( "Classes", "/classes" ) ]

        ClassPage submodel ->
            [ ( "Classes", "/classes" ), ( ClassPage.getPageTitle submodel, ClassPage.getPageLink submodel.id ) ]

        ClassManageTutorsPage submodel ->
            ClassManageTutorsPage.getNestedNavigation submodel

        ClassAttendancePage submodel ->
            ClassAttendancePage.getNestedNavigation submodel

        ClassEditPage submodel ->
            case submodel.id of
                Nothing ->
                    [ ( ClassEditPage.getPageTitle submodel, ClassEditPage.getPageLink submodel ) ]

                Just classId ->
                    [ ( "Class Profile", ClassPage.getPageLink classId )
                    , ( ClassEditPage.getPageTitle submodel, ClassEditPage.getPageLink submodel )
                    ]


getCredentials : Model -> Maybe Api.Credentials
getCredentials model =
    case model of
        LoggedOut submodel ->
            submodel.credentials

        Home submodel ->
            Just submodel.credentials

        TutorListPage submodel ->
            Just submodel.credentials

        TutorPage submodel ->
            Just submodel.credentials

        TutorEditPage submodel ->
            Just submodel.credentials

        StudentListPage submodel ->
            Just submodel.credentials

        StudentPage submodel ->
            Just submodel.credentials

        StudentEditPage submodel ->
            Just submodel.credentials

        ClassPage submodel ->
            Just submodel.credentials

        ClassListPage submodel ->
            Just submodel.credentials

        ClassManageTutorsPage submodel ->
            Just submodel.credentials

        ClassAttendancePage submodel ->
            Just submodel.credentials

        ClassEditPage submodel ->
            Just submodel.credentials


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

        StudentListPage submodel ->
            submodel.key

        StudentPage submodel ->
            submodel.key

        StudentEditPage submodel ->
            submodel.key

        ClassListPage submodel ->
            submodel.key

        ClassPage submodel ->
            submodel.key

        ClassManageTutorsPage submodel ->
            submodel.key

        ClassAttendancePage submodel ->
            submodel.key

        TutorEditPage submodel ->
            submodel.key

        ClassEditPage submodel ->
            submodel.key


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        key =
            getNavigationKey model

        maybeCredentials =
            getCredentials model
    in
    case maybeCredentials of
        Nothing ->
            ( LoggedOut (Login.init key), Cmd.none )

        Just credentials ->
            case Maybe.withDefault NotFound (UrlParser.parse routeParser url) of
                RouteHome ->
                    Home.init credentials key
                        |> Tuple.mapFirst Home
                        |> Tuple.mapSecond (Cmd.map GotHomeMsg)

                RouteTutors filters maybePage ->
                    TutorListPage.init credentials key filters (maybePage |> Maybe.withDefault 0)
                        |> Tuple.mapFirst TutorListPage
                        |> Tuple.mapSecond (Cmd.map GotTutorListMsg)

                RouteTutorNew ->
                    TutorEditPage.initWithEmpty credentials key
                        |> Tuple.mapFirst TutorEditPage
                        |> Tuple.mapSecond (Cmd.map GotTutorEditMsg)

                RouteTutor id ->
                    TutorPage.init credentials key id
                        |> Tuple.mapFirst TutorPage
                        |> Tuple.mapSecond (Cmd.map GotTutorMsg)

                RouteTutorEdit id ->
                    TutorEditPage.initWithTutor credentials key id
                        |> Tuple.mapFirst TutorEditPage
                        |> Tuple.mapSecond (Cmd.map GotTutorEditMsg)

                RouteStudents filters maybePage ->
                    StudentListPage.init credentials key filters (maybePage |> Maybe.withDefault 0)
                        |> Tuple.mapFirst StudentListPage
                        |> Tuple.mapSecond (Cmd.map GotStudentListMsg)

                RouteStudentNew ->
                    StudentEditPage.initWithEmpty credentials key
                        |> Tuple.mapFirst StudentEditPage
                        |> Tuple.mapSecond (Cmd.map GotStudentEditMsg)

                RouteStudent id ->
                    StudentPage.init credentials key id
                        |> Tuple.mapFirst StudentPage
                        |> Tuple.mapSecond (Cmd.map GotStudentMsg)

                RouteStudentEdit id ->
                    StudentEditPage.initWithStudent credentials key id
                        |> Tuple.mapFirst StudentEditPage
                        |> Tuple.mapSecond (Cmd.map GotStudentEditMsg)

                RouteClasses filters maybePage ->
                    ClassListPage.init credentials key filters (maybePage |> Maybe.withDefault 0)
                        |> Tuple.mapFirst ClassListPage
                        |> Tuple.mapSecond (Cmd.map GotClassListMsg)

                RouteClass id ->
                    ClassPage.init id credentials key
                        |> Tuple.mapFirst ClassPage
                        |> Tuple.mapSecond (Cmd.map GotClassMsg)

                RouteClassManageTutors id ->
                    ClassManageTutorsPage.init credentials key id
                        |> Tuple.mapFirst ClassManageTutorsPage
                        |> Tuple.mapSecond (Cmd.map GotClassManageTutorMsg)

                RouteClassAttendance classId sessionId ->
                    ClassAttendancePage.init credentials key classId sessionId
                        |> Tuple.mapFirst ClassAttendancePage
                        |> Tuple.mapSecond (Cmd.map GotClassAttendanceMsg)

                RouteClassEdit classId ->
                    ClassEditPage.initWithClass credentials key classId
                        |> Tuple.mapFirst ClassEditPage
                        |> Tuple.mapSecond (Cmd.map GotClassEditMsg)

                RouteClassNew ->
                    ClassEditPage.initWithEmpty credentials key
                        |> Tuple.mapFirst ClassEditPage
                        |> Tuple.mapSecond (Cmd.map GotClassEditMsg)

                RouteLogout ->
                    ( Login.init key, Cmd.none )
                        |> Tuple.mapFirst LoggedOut
                        |> Tuple.mapSecond (Cmd.map GotLoginMsg)

                NotFound ->
                    Home.init credentials key
                        |> Tuple.mapFirst Home
                        |> Tuple.mapSecond (Cmd.map GotHomeMsg)


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( LoggedOut (Login.init key), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )

        -- _ =
        --     Debug.log "MSG" (Debug.toString msg)
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

        GotTutorEditMsg subMsg ->
            case model of
                TutorEditPage submodel ->
                    let
                        ( newModel, newMsg ) =
                            TutorEditPage.update subMsg submodel
                    in
                    ( TutorEditPage newModel, Cmd.map GotTutorEditMsg newMsg )

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

        GotClassManageTutorMsg subMsg ->
            case model of
                ClassManageTutorsPage submodel ->
                    case ClassManageTutorsPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( ClassManageTutorsPage newModel, Cmd.map GotClassManageTutorMsg newMsg )

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

        GotClassEditMsg subMsg ->
            case model of
                ClassEditPage submodel ->
                    let
                        ( newModel, newMsg ) =
                            ClassEditPage.update subMsg submodel
                    in
                    ( ClassEditPage newModel, Cmd.map GotClassEditMsg newMsg )

                _ ->
                    ignore

        GotStudentEditMsg subMsg ->
            case model of
                StudentEditPage submodel ->
                    let
                        ( newModel, newMsg ) =
                            StudentEditPage.update subMsg submodel
                    in
                    ( StudentEditPage newModel, Cmd.map GotStudentEditMsg newMsg )

                _ ->
                    ignore

        GotStudentListMsg subMsg ->
            case model of
                StudentListPage submodel ->
                    case StudentListPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( StudentListPage newModel, Cmd.map GotStudentListMsg newMsg )

                _ ->
                    ignore

        GotStudentMsg subMsg ->
            case model of
                StudentPage submodel ->
                    case StudentPage.update subMsg submodel of
                        ( newModel, newMsg ) ->
                            ( StudentPage newModel, Cmd.map GotStudentMsg newMsg )

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
        , Element.spacing 5
        , Background.color Colors.theme.p500
        , Font.color Colors.black
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height (Element.px 100)
            , Element.padding 10
            ]
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Background.uncropped "src/Logo.png"
                ]
                Element.none
            )
        , viewDrawerElement "Home" "/home"
        , viewDrawerElement "Tutors" "/tutors"
        , viewDrawerElement "Students" "/students"
        , viewDrawerElement "Classes" "/classes"
        , viewDrawerElement "Logout" "/logout"
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
        , Background.color Colors.theme.p500
        , Element.spacing 20
        , Element.paddingEach { top = 20, left = 50, right = 50, bottom = 10 }
        , Font.size 18
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

                TutorEditPage submodel ->
                    viewWrapped model <| Element.map GotTutorEditMsg (TutorEditPage.view submodel)

                StudentListPage submodel ->
                    viewWrapped model <| Element.map GotStudentListMsg (StudentListPage.view submodel)

                StudentPage submodel ->
                    viewWrapped model <| Element.map GotStudentMsg (StudentPage.view submodel)

                StudentEditPage submodel ->
                    viewWrapped model <| Element.map GotStudentEditMsg (StudentEditPage.view submodel)

                ClassListPage submodel ->
                    viewWrapped model <| Element.map GotClassListMsg (ClassListPage.view submodel)

                ClassPage submodel ->
                    viewWrapped model <| Element.map GotClassMsg (ClassPage.view submodel)

                ClassManageTutorsPage submodel ->
                    viewWrapped model <| Element.map GotClassManageTutorMsg (ClassManageTutorsPage.view submodel)

                ClassAttendancePage submodel ->
                    viewWrapped model <| Element.map GotClassAttendanceMsg (ClassAttendancePage.view submodel)

                ClassEditPage submodel ->
                    viewWrapped model <| Element.map GotClassEditMsg (ClassEditPage.view submodel)
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
