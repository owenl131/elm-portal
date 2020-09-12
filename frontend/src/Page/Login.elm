module Page.Login exposing (Model, Msg, init, update, view)

import Element exposing (Element)
import Element.Input as Input
import Browser.Navigation as Navigation


type alias Model =
    { key : Navigation.Key
    , email : String
    , password : String
    }


type Msg
    = EnteredEmail String
    | EnteredPassword String
    | SubmittedForm


init : Navigation.Key -> Model
init key =
    { key = key
    , email = ""
    , password = ""
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
            ( model, Navigation.pushUrl model.key "/home" )


view : Model -> Element Msg
view model =
    Element.el
        [ Element.padding 50 ]
    <|
        Element.column
            []
            [ Input.email
                []
                { onChange = EnteredEmail
                , text = model.email
                , placeholder = Just <| Input.placeholder [] (Element.text "Email")
                , label = Input.labelAbove [] (Element.text "Email")
                }
            , Input.currentPassword
                []
                { onChange = EnteredPassword
                , text = model.password
                , placeholder = Just <| Input.placeholder [] (Element.text "Password")
                , label = Input.labelAbove [] (Element.text "Password")
                , show = False
                }
            , Input.button
                []
                { onPress = Just SubmittedForm
                , label = Element.text "Login"
                }
            ]
