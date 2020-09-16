module Page.Class.AddTutor exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Date
import DatePicker
import Element exposing (Element)
import Element.Input as Input
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import Tutor exposing (Tutor)


type alias Model =
    { key : Navigation.Key
    , id : Int
    , tutors : WebData (List Tutor)
    , nameFilter : String
    , joinDate : Maybe Date.Date
    , joinDatePicker : DatePicker.Model
    , suggestions : WebData (List Tutor)
    }


type Msg
    = GotTutorSuggestionList (Result Http.Error (List Tutor))
    | GotTutorList (Result Http.Error (List Tutor))
    | AddTutor String
    | GotAddTutorResult (Result Http.Error ())
    | EnteredNameFilter String
    | FetchSuggestions
    | PickerChanged DatePicker.ChangeEvent


getPageTitle : Model -> String
getPageTitle _ =
    "Add Tutors"


getPageLink : Model -> String
getPageLink model =
    "/class/" ++ String.fromInt model.id ++ "/addtutor"


init : Navigation.Key -> Int -> ( Model, Cmd Msg )
init key id =
    ( { key = key
      , id = id
      , tutors = RemoteData.Loading
      , nameFilter = ""
      , suggestions = RemoteData.Loading
      , joinDate = Nothing
      , joinDatePicker = DatePicker.init
      }
    , Cmd.none
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
        , expect = Http.expectJson GotTutorList (Decode.list Tutor.tutorDecoder)
        }


postAddTutor : Int -> String -> Cmd Msg
postAddTutor classId tutorId =
    Http.post
        { url = "http://localhost:5000/class/" ++ String.fromInt classId ++ "/addtutor/" ++ tutorId
        , body = Http.emptyBody
        , expect = Http.expectWhatever GotAddTutorResult
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        EnteredNameFilter filter ->
            ( { model | nameFilter = filter }, Cmd.none )

        FetchSuggestions ->
            ( model, fetchSuggestions model.nameFilter )

        GotTutorSuggestionList result ->
            ( { model | suggestions = RemoteData.fromResult result }, Cmd.none )

        GotTutorList result ->
            ( { model | tutors = RemoteData.fromResult result }, Cmd.none )

        GotAddTutorResult _ ->
            ( model, fetchTutorList model.id )

        AddTutor tutorId ->
            ( model, postAddTutor model.id tutorId )

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
        , DatePicker.input []
            { label = Input.labelLeft [] (Element.text "Joined on")
            , model = model.joinDatePicker
            , onChange = PickerChanged
            , placeholder = Just (Input.placeholder [] (Element.text "Unselected"))
            , selected = model.joinDate
            , settings = DatePicker.defaultSettings
            , text = Maybe.map Date.toIsoString model.joinDate |> Maybe.withDefault "Joined on"
            }
        , case model.suggestions of
            RemoteData.Loading ->
                Element.text "Loading"

            RemoteData.NotAsked ->
                Element.text "Not asked"

            RemoteData.Failure err ->
                Element.text (Debug.toString err)

            RemoteData.Success data ->
                viewSuggestions data
        ]


viewList : Model -> Element Msg
viewList _ =
    Element.text "List"


view : Model -> Element Msg
view model =
    Element.row
        [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.el
            [ Element.height Element.fill, Element.width Element.fill ]
            (viewSelector model)
        , Element.el
            [ Element.height Element.fill, Element.width Element.fill ]
            (viewList model)
        ]
