module Page.Class.AddTutor exposing
    ( Model
    , Msg
    , getNestedNavigation
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
import DatePicker
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Task
import Tutor exposing (Tutor)
import Url.Builder as Builder


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Class.ClassId
    , tutors : WebData (List ClassTutor)
    , nameFilter : String
    , classData : WebData Class.Class
    , joinDate : Maybe Date.Date
    , joinDatePicker : DatePicker.Model
    , suggestions : WebData (List TutorSuggestion)
    }


type Msg
    = GotTutorSuggestionList (Result Http.Error (List TutorSuggestion))
    | GotTutorList (Result Http.Error (List ClassTutor))
    | GotClassData (Result Http.Error Class.Class)
    | AddTutor String
    | GotAddTutorResult (Result Http.Error ())
    | EnteredNameFilter String
    | FetchSuggestions
    | PickerChanged DatePicker.ChangeEvent
    | SetToday Date.Date


type alias TutorSuggestion =
    { id : String
    , name : String
    , admin : Tutor.AdminLevel
    }


tutorSuggestionDecoder : Decode.Decoder TutorSuggestion
tutorSuggestionDecoder =
    Decode.succeed TutorSuggestion
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "admin" Tutor.tutorAdminLevelDecoder


getPageTitle : Model -> String
getPageTitle _ =
    "Add Tutors"


getPageLink : Model -> String
getPageLink model =
    Builder.absolute [ "class", model.id, "addtutor" ] []


getNestedNavigation : Model -> List ( String, String )
getNestedNavigation model =
    [ ( "Classes", "/classes" )
    , ( RemoteData.toMaybe model.classData
            |> Maybe.map .name
            |> Maybe.withDefault ("Class ID: " ++ model.id)
      , "/class/" ++ model.id
      )
    , ( getPageTitle model, getPageLink model )
    ]


init : Api.Credentials -> Navigation.Key -> Class.ClassId -> ( Model, Cmd Msg )
init credentials key id =
    let
        model =
            { key = key
            , credentials = credentials
            , id = id
            , tutors = RemoteData.Loading
            , nameFilter = ""
            , suggestions = RemoteData.Loading
            , classData = RemoteData.Loading
            , joinDate = Nothing
            , joinDatePicker = DatePicker.init
            }
    in
    ( model
    , Cmd.batch
        [ fetchClassDetails model.credentials model.id
        , fetchSuggestions model.credentials model.id model.nameFilter
        , fetchTutorList model.credentials model.id
        , Task.perform SetToday Date.today
        ]
    )


fetchSuggestions : Api.Credentials -> Class.ClassId -> String -> Cmd Msg
fetchSuggestions credentials classId arg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "suggestions" ] [ Builder.string "filter" arg ]
        , expect = Http.expectJson GotTutorSuggestionList (Decode.list tutorSuggestionDecoder)
        }


fetchTutorList : Api.Credentials -> Class.ClassId -> Cmd Msg
fetchTutorList credentials classId =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "tutors" ] []
        , expect = Http.expectJson GotTutorList (Decode.list Class.classTutorDecoder)
        }


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


postAddTutor : Api.Credentials -> Class.ClassId -> String -> Date.Date -> Cmd Msg
postAddTutor credentials classId tutorId joinDate =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "addtutor" ] []
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "tutorId", Encode.string tutorId )
                    , ( "joinDate", Encode.string (Date.toIsoString joinDate) )
                    ]
                )
        , expect = Http.expectWhatever GotAddTutorResult
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        SetToday date ->
            ( { model
                | joinDatePicker = DatePicker.setToday date model.joinDatePicker
                , joinDate = Just date
              }
            , Cmd.none
            )

        EnteredNameFilter filter ->
            ( { model | nameFilter = filter }, Cmd.none )

        FetchSuggestions ->
            ( model, fetchSuggestions model.credentials model.id model.nameFilter )

        GotTutorSuggestionList result ->
            ( { model | suggestions = RemoteData.fromResult result }, Cmd.none )

        GotTutorList result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        GotClassData result ->
            ( { model | classData = RemoteData.fromResult result }, Cmd.none )

        GotAddTutorResult _ ->
            ( { model | nameFilter = "" }
            , Cmd.batch
                [ fetchTutorList model.credentials model.id
                , fetchSuggestions model.credentials model.id ""
                ]
            )

        AddTutor tutorId ->
            case model.joinDate of
                Nothing ->
                    ignore

                Just joinDate ->
                    ( model, postAddTutor model.credentials model.id tutorId joinDate )

        PickerChanged changeEvent ->
            case changeEvent of
                DatePicker.DateChanged date ->
                    ( { model | joinDate = Just date }, Cmd.none )

                DatePicker.TextChanged text ->
                    ( { model | joinDate = Date.fromIsoString text |> Result.toMaybe }, Cmd.none )

                DatePicker.PickerChanged subMsg ->
                    ( { model | joinDatePicker = DatePicker.update subMsg model.joinDatePicker }, Cmd.none )


viewSuggestions : List TutorSuggestion -> Element Msg
viewSuggestions tutors =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 10, left = 0, right = 20, bottom = 5 } ]
    in
    -- for each suggestion, display name, admin, and button to add
    Element.table
        [ Element.spacing 5
        ]
        { data = tutors
        , columns =
            [ { header = "Name" |> toHeader
              , width = Element.fill |> Element.maximum 150
              , view = .name >> Element.text >> Element.el [ Element.centerY ]
              }
            , { header = "Role" |> toHeader
              , width = Element.fill |> Element.maximum 80
              , view = .admin >> Tutor.adminLevelAsString >> Element.text >> Element.el [ Element.centerY ]
              }
            , { header = "Add" |> toHeader
              , width = Element.fill |> Element.maximum 50
              , view =
                    \tutor ->
                        Input.button
                            [ Background.color Colors.theme.a400
                            , Border.width 1
                            , Border.rounded 3
                            , Element.paddingXY 10 2
                            , Element.mouseOver [ Background.color Colors.theme.a200 ]
                            ]
                            { label = Element.text "+" |> Element.el [ Element.centerX ], onPress = Just (AddTutor tutor.id) }
              }
            ]
        }


viewSelector : Model -> Element Msg
viewSelector model =
    Element.column
        [ Element.height Element.fill
        , Element.spacing 10
        , Element.padding 20
        , Background.color Colors.theme.p50
        ]
        [ Input.text [ Element.padding 4, Element.width (Element.px 200) ]
            { label = Input.labelLeft [] (Element.text "Filter by Name" |> Element.el [ Element.width (Element.px 150) ])
            , onChange = EnteredNameFilter
            , placeholder = Nothing
            , text = model.nameFilter
            }
        , Input.button
            [ Background.color Colors.theme.a400
            , Border.width 1
            , Border.rounded 3
            , Element.paddingXY 10 2
            , Element.mouseOver [ Background.color Colors.theme.a200 ]
            ]
            { label = Element.text "Search", onPress = Just FetchSuggestions }
        , Element.el [ Element.height (Element.px 20) ] Element.none
        , DatePicker.input [ Element.padding 4, Element.width (Element.px 120) ]
            { label = Input.labelLeft [] (Element.text "Set Joined On" |> Element.el [ Element.width (Element.px 150) ])
            , model = model.joinDatePicker
            , onChange = PickerChanged
            , placeholder = Just (Input.placeholder [] (Element.text "Unselected"))
            , selected = model.joinDate
            , settings = DatePicker.defaultSettings
            , text = Maybe.map Date.toIsoString model.joinDate |> Maybe.withDefault "Joined on"
            }
        , handleRemote viewSuggestions model.suggestions
        ]


viewList : List ClassTutor -> Element Msg
viewList tutors =
    let
        toHeader : String -> Element Msg
        toHeader text =
            text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, left = 0, right = 20, bottom = 5 } ]
    in
    Element.table
        [ Element.padding 20
        , Border.color Colors.theme.p50
        , Border.width 3
        , Element.spacing 5
        ]
        { data = tutors
        , columns =
            [ { header = "Name" |> toHeader
              , width = Element.fill |> Element.maximum 150 |> Element.minimum 100
              , view = .name >> Element.text
              }
            , { header = "Joined Class on" |> toHeader
              , width = Element.fill |> Element.maximum 100
              , view = .joinDate >> Date.toIsoString >> Element.text
              }
            ]
        }


handleRemote : (a -> Element Msg) -> WebData a -> Element Msg
handleRemote viewIt remoteData =
    case remoteData of
        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.NotAsked ->
            Element.text "Not asked"

        RemoteData.Failure err ->
            Element.text (Debug.toString err)

        RemoteData.Success data ->
            viewIt data


view : Model -> Element Msg
view model =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 10
        ]
        [ Element.el
            [ Element.height Element.fill ]
            (viewSelector model)
        , Element.el
            [ Element.height Element.fill ]
            (handleRemote viewList model.tutors)
        ]
