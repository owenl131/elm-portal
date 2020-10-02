module Page.Tutor exposing (Model, Msg, getPageLink, getPageTitle, init, update, view)

import Api
import Browser.Navigation
import Element exposing (Element)
import Http
import RemoteData exposing (WebData)
import Tutor exposing (Tutor, tutorDecoder)


type alias Model =
    { key : Browser.Navigation.Key
    , credentials : Api.Credentials
    , id : String
    , data : WebData Tutor
    }


type Msg
    = GotTutorData (Result Http.Error Tutor)


getPageTitle : Model -> String
getPageTitle model =
    RemoteData.toMaybe model.data |> Maybe.map .name |> Maybe.withDefault "Tutor"


getPageLink : String -> String
getPageLink id =
    "/tutor/" ++ id


init : Api.Credentials -> Browser.Navigation.Key -> String -> ( Model, Cmd Msg )
init credentials key id =
    ( { key = key, credentials = credentials, id = id, data = RemoteData.Loading }
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
            -- Display tutor classes
            -- Display tutor recently attended sessions
            -- Display tutor hours with breakdown by class
            -- Display tutor trainings
            -- Display chart of attended sessions over last year
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
