module Page.Class.AddTutor exposing
    ( Model
    , Msg
    , getNestedNavigation
    , init
    , update
    , view
    )

import Browser.Navigation as Navigation
import Class exposing (ClassTutor)
import Date
import DatePicker
import Element exposing (Element)
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Task
import Tutor exposing (Tutor)


type alias Model =
    { key : Navigation.Key
    , id : Int
    , tutors : WebData (List ClassTutor)
    , nameFilter : String
    , classData : WebData Class.Class
    , joinDate : Maybe Date.Date
    , joinDatePicker : DatePicker.Model
    , suggestions : WebData (List Tutor)
    }


type Msg
    = GotTutorSuggestionList (Result Http.Error (List Tutor))
    | GotTutorList (Result Http.Error (List ClassTutor))
    | GotClassData (Result Http.Error Class.Class)
    | AddTutor String
    | GotAddTutorResult (Result Http.Error ())
    | EnteredNameFilter String
    | FetchSuggestions
    | PickerChanged DatePicker.ChangeEvent
    | SetToday Date.Date


getPageTitle : Model -> String
getPageTitle _ =
    "Add Tutors"


getPageLink : Model -> String
getPageLink model =
    "/class/" ++ String.fromInt model.id ++ "/addtutor"


getNestedNavigation : Model -> List ( String, String )
getNestedNavigation model =
    [ ( "Classes", "/classes" )
    , ( RemoteData.toMaybe model.classData
            |> Maybe.map .name
            |> Maybe.withDefault ("Class ID: " ++ String.fromInt model.id)
      , "/class/" ++ String.fromInt model.id
      )
    , ( getPageTitle model, getPageLink model )
    ]


init : Navigation.Key -> Int -> ( Model, Cmd Msg )
init key id =
    let
        model =
            { key = key
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
        [ fetchClassDetails model
        , fetchSuggestions model.nameFilter
        , fetchTutorList model.id
        , Task.perform SetToday Date.today
        ]
    )


fetchSuggestions : String -> Cmd Msg
fetchSuggestions arg =
    Http.get
        { url = "http://localhost:5000/suggestions?filter=" ++ arg
        , expect = Http.expectJson GotTutorSuggestionList (Decode.list Tutor.tutorDecoder)
        }


fetchTutorList : Int -> Cmd Msg
fetchTutorList classId =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt classId ++ "/tutors"
        , expect = Http.expectJson GotTutorList (Decode.list Class.classTutorDecoder)
        }


fetchClassDetails : Model -> Cmd Msg
fetchClassDetails model =
    Http.get
        { url = "http://localhost:5000/class/" ++ String.fromInt model.id
        , expect = Http.expectJson GotClassData Class.classDecoder
        }


postAddTutor : Int -> String -> Date.Date -> Cmd Msg
postAddTutor classId tutorId joinDate =
    Http.post
        { url =
            "http://localhost:5000/class/"
                ++ String.fromInt classId
                ++ "/addtutor/"
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
            ( model, fetchSuggestions model.nameFilter )

        GotTutorSuggestionList result ->
            ( { model | suggestions = RemoteData.fromResult result }, Cmd.none )

        GotTutorList result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        GotClassData result ->
            ( { model | classData = RemoteData.fromResult result }, Cmd.none )

        GotAddTutorResult _ ->
            ( { model | nameFilter = "" }
            , Cmd.batch
                [ fetchTutorList model.id
                , fetchSuggestions ""
                ]
            )

        AddTutor tutorId ->
            case model.joinDate of
                Nothing ->
                    ignore

                Just joinDate ->
                    ( model, postAddTutor model.id tutorId joinDate )

        PickerChanged changeEvent ->
            case changeEvent of
                DatePicker.DateChanged date ->
                    ( { model | joinDate = Just date }, Cmd.none )

                DatePicker.TextChanged text ->
                    ( { model | joinDate = Date.fromIsoString text |> Result.toMaybe }, Cmd.none )

                DatePicker.PickerChanged subMsg ->
                    ( { model | joinDatePicker = DatePicker.update subMsg model.joinDatePicker }, Cmd.none )


viewSuggestions : List Tutor -> Element Msg
viewSuggestions tutors =
    -- for each suggestion, display name, admin, and button to add
    Element.table []
        { data = tutors
        , columns =
            [ { header = Element.text "Name"
              , width = Element.fill
              , view = .name >> Element.text
              }
            , { header = Element.text "Add"
              , width = Element.fill
              , view = \tutor -> Input.button [] { label = Element.text "+", onPress = Just (AddTutor tutor.id) }
              }
            ]
        }


viewSelector : Model -> Element Msg
viewSelector model =
    Element.column
        [ Element.height Element.fill ]
        [ Input.text []
            { label = Input.labelLeft [] (Element.text "Filter by Name")
            , onChange = EnteredNameFilter
            , placeholder = Nothing
            , text = model.nameFilter
            }
        , Input.button [] { label = Element.text "Search", onPress = Just FetchSuggestions }
        , DatePicker.input []
            { label = Input.labelLeft [] (Element.text "Joined on")
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
    Element.table
        []
        { data = tutors
        , columns =
            [ { header = Element.text "Name"
              , width = Element.fill
              , view = .name >> Element.text
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
        [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.el
            [ Element.height Element.fill, Element.width Element.fill ]
            (viewSelector model)
        , Element.el
            [ Element.height Element.fill, Element.width Element.fill ]
            (handleRemote viewList model.tutors)
        ]
