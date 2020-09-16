module Page.Tutor exposing (Model, Msg, getPageLink, getPageTitle, init, update, view)

import Browser.Navigation
import Element exposing (Element)
import Http
import RemoteData exposing (WebData)
import Tutor exposing (Tutor, tutorDecoder)


type alias Model =
    { key : Browser.Navigation.Key
    , id : String
    , data : WebData Tutor
    }


type Msg
    = GotTutorData (Result Http.Error Tutor)


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.data |> Maybe.map .name |> Maybe.withDefault "Tutor"


getPageLink : Model -> String
getPageLink model =
    "/tutor/" ++ model.id


init : Browser.Navigation.Key -> String -> ( Model, Cmd Msg )
init key id =
    ( { key = key, id = id, data = RemoteData.Loading }
    , Http.get
        { url = "http://localhost:5000/tutor/" ++ id
        , expect = Http.expectJson GotTutorData tutorDecoder
        }
    )


viewRow : String -> Tutor -> (Tutor -> String) -> Element Msg
viewRow label tutor accessor =
    Element.row
        []
        [ Element.text label
        , Element.text (accessor tutor)
        ]


view : Model -> Element Msg
view model =
    case model.data of
        RemoteData.NotAsked ->
            Element.text "Not Asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure err ->
            Element.text (Debug.toString err)

        RemoteData.Success data ->
            Element.column
                []
                [ viewRow "Name" data .name
                , viewRow "Email" data .email
                , viewRow "School" data .school
                , viewRow "School" data .school
                , viewRow "School" data .school
                , viewRow "School" data .school
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTutorData result ->
            case result of
                Ok data ->
                    ( { model | data = RemoteData.Success data }, Cmd.none )

                Err error ->
                    ( { model | data = RemoteData.Failure error }, Cmd.none )
