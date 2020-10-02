module Page.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Encode as Encode
import RemoteData exposing (WebData)


type alias Model =
    { key : Navigation.Key
    , email : String
    , password : String
    , attempt : WebData ()
    }


type Msg
    = EnteredEmail String
    | EnteredPassword String
    | SubmittedForm
    | GotAuthResult (Result Http.Error ())


init : Navigation.Key -> Model
init key =
    { key = key
    , email = ""
    , password = ""
    , attempt = RemoteData.NotAsked
    }


authenticate : String -> String -> Cmd Msg
authenticate email password =
    Http.post
        { url = "http://localhost:8001/backend/auth"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
                )
        , expect = Http.expectWhatever GotAuthResult
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        SubmittedForm ->
            -- make HTTP request
            ( model, authenticate model.email model.password )

        GotAuthResult result ->
            let
                data =
                    RemoteData.fromResult result
            in
            case data of
                RemoteData.Success _ ->
                    ( model, Navigation.pushUrl model.key "/home" )

                _ ->
                    ( { model | attempt = data }, Cmd.none )


view : Model -> Element Msg
view model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Colors.theme.p50
        ]
        (Element.column
            [ Background.color Colors.theme.p300
            , Element.centerX
            , Element.centerY
            , Element.padding 50
            , Element.spacing 10
            ]
            [ Element.el [ Font.bold, Font.size 18 ] (Element.text "Elm Portal v0.1")
            , Element.el [ Element.height (Element.px 20) ] Element.none
            , Input.email
                [ Element.padding 8 ]
                { onChange = EnteredEmail
                , text = model.email
                , placeholder = Just (Input.placeholder [] (Element.text "Email"))
                , label = Input.labelLeft [ Element.width (Element.px 100) ] (Element.text "Email")
                }
            , Input.currentPassword
                [ Element.padding 8 ]
                { onChange = EnteredPassword
                , text = model.password
                , placeholder = Just (Input.placeholder [] (Element.text "Password"))
                , label = Input.labelLeft [ Element.width (Element.px 100) ] (Element.text "Password")
                , show = False
                }
            , Element.el [ Element.height (Element.px 20) ] Element.none
            , let
                disabled =
                    model.email == "" || model.password == ""
              in
              Input.button
                [ Background.color
                    (if disabled then
                        Colors.grey

                     else
                        Colors.theme.a400
                    )
                , Element.paddingXY 50 8
                , Border.width 2
                , Border.color Colors.black
                , Border.rounded 20
                , Element.centerX
                , Element.mouseOver
                    (if disabled then
                        []

                     else
                        [ Background.color Colors.theme.a200 ]
                    )
                ]
                { onPress =
                    if disabled then
                        Nothing

                    else
                        Just SubmittedForm
                , label = Element.text "Login"
                }
            , Element.el [ Element.padding 10 ] Element.none
            , case model.attempt of
                RemoteData.NotAsked ->
                    Element.none

                RemoteData.Loading ->
                    Element.text "Checking password..."

                RemoteData.Failure err ->
                    Element.text ("Invalid username or password!" ++ Debug.toString err)

                RemoteData.Success _ ->
                    Element.text "Logged in successfully!"
            ]
        )
