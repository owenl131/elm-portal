module Page.Tutor.Edit exposing
    ( Model
    , Msg
    , getPageLink
    , getPageTitle
    , initWithEmpty
    , initWithTutor
    , update
    , view
    )

import Api
import Base64
import Browser.Navigation as Navigation
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
import Maybe exposing (withDefault)
import Maybe.Extra
import Page.Tutor exposing (getPageLink)
import Regex
import Styles
import Task
import Tutor exposing (AdminLevel, Gender, Tutor, TutorStatus, emptyTutor)
import Url.Builder as Builder
import Validate


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Maybe String
    , data : Tutor
    , formState : FormState
    , errorMessage : Maybe String
    , successMessage : Maybe String
    }


type WhichFormElement
    = Dob
    | Doc


type alias FormState =
    { dobPicker : DatePicker.Model
    , dobText : String
    , docPicker : DatePicker.Model
    , docText : String
    }


type Msg
    = NameChanged String
    | EmailChanged String
    | SchoolChanged String
    | GenderChanged Gender
    | StatusChanged TutorStatus
    | AdminChanged AdminLevel
    | PasswordChanged String
    | PickerChanged WhichFormElement DatePicker.ChangeEvent
    | SetToday Date.Date
    | GotTutorData (Result Http.Error Tutor)
    | GotUpdated (Result Http.Error ())
    | GotTutorAdded (Result Http.Error String)
    | ToProfile
    | Submit


getPageTitle : Model -> String
getPageTitle model =
    case model.id of
        Nothing ->
            "New Tutor"

        Just tutorId ->
            "Edit: " ++ model.data.name


getPageLink : Model -> String
getPageLink model =
    case model.id of
        Nothing ->
            Builder.absolute [ "tutors", "new" ] []

        Just tutorId ->
            Builder.absolute [ "tutor", tutorId, "edit" ] []


postNewTutor : Api.Credentials -> Tutor -> Cmd Msg
postNewTutor credentials tutor =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Tutor.tutorEncoder tutor)
        , url = Builder.crossOrigin Api.endpoint [ "tutors", "new" ] []
        , expect = Http.expectJson GotTutorAdded (Decode.field "id" Decode.string)
        , timeout = Nothing
        , tracker = Nothing
        }


postTutorUpdate : Api.Credentials -> Tutor -> Cmd Msg
postTutorUpdate credentials tutor =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Tutor.tutorEncoder tutor)
        , url = Builder.crossOrigin Api.endpoint [ "tutor", tutor.id ] []
        , expect = Http.expectWhatever GotUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


initWithEmpty : Api.Credentials -> Navigation.Key -> ( Model, Cmd Msg )
initWithEmpty credentials key =
    ( { key = key
      , credentials = credentials
      , id = Nothing
      , data = Tutor.emptyTutor
      , formState =
            { dobPicker = DatePicker.init
            , docPicker = DatePicker.init
            , dobText = ""
            , docText = ""
            }
      , errorMessage = Nothing
      , successMessage = Nothing
      }
    , Task.perform SetToday Date.today
    )


initWithTutor : Api.Credentials -> Navigation.Key -> String -> ( Model, Cmd Msg )
initWithTutor credentials key id =
    ( { key = key
      , credentials = credentials
      , id = Just id
      , data = Tutor.emptyTutor
      , formState =
            { dobPicker = DatePicker.init
            , docPicker = DatePicker.init
            , dobText = ""
            , docText = ""
            }
      , errorMessage = Nothing
      , successMessage = Nothing
      }
    , Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "tutor", id ] []
        , expect = Http.expectJson GotTutorData Tutor.tutorDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
    )


isValidPassword : Bool -> Maybe String -> Bool
isValidPassword isNew maybePassword =
    case maybePassword of
        Nothing ->
            if isNew then
                False

            else
                True

        Just password ->
            String.length password >= 8


isValidEmail : String -> Bool
isValidEmail email =
    Regex.contains
        (Regex.fromStringWith
            { caseInsensitive = True, multiline = False }
            "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[A-Z0-9-]+\\.)+[A-Z]{2,6}$"
            |> withDefault Regex.never
        )
        email


updateFormStatePicker : WhichFormElement -> (DatePicker.Model -> DatePicker.Model) -> FormState -> FormState
updateFormStatePicker which updater form =
    case which of
        Dob ->
            { form | dobPicker = updater form.dobPicker }

        Doc ->
            { form | docPicker = updater form.docPicker }


updateFormStateText : WhichFormElement -> String -> FormState -> FormState
updateFormStateText which newText form =
    case which of
        Dob ->
            { form | dobText = newText }

        Doc ->
            { form | docText = newText }


updateTutor : WhichFormElement -> Date.Date -> Tutor -> Tutor
updateTutor which date tutor =
    case which of
        Dob ->
            { tutor | dateOfBirth = date }

        Doc ->
            { tutor | dateOfRegistration = date }


getTutorField : WhichFormElement -> Tutor -> Date.Date
getTutorField which tutor =
    case which of
        Dob ->
            tutor.dateOfBirth

        Doc ->
            tutor.dateOfRegistration


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
    case msg of
        GotTutorData result ->
            case Result.toMaybe result of
                Nothing ->
                    ( { model | data = emptyTutor }, Cmd.none )

                Just tutor ->
                    ( { model
                        | data = tutor
                        , formState =
                            model.formState
                                |> updateFormStateText Dob (Date.toIsoString tutor.dateOfBirth)
                                |> updateFormStateText Doc (Date.toIsoString tutor.dateOfRegistration)
                                |> updateFormStatePicker Dob (DatePicker.setVisibleMonth tutor.dateOfBirth)
                                |> updateFormStatePicker Doc (DatePicker.setVisibleMonth tutor.dateOfRegistration)
                      }
                    , Cmd.none
                    )

        NameChanged value ->
            ( { model | data = { data | name = value } }, Cmd.none )

        EmailChanged value ->
            ( { model | data = { data | email = value } }, Cmd.none )

        SchoolChanged value ->
            ( { model | data = { data | school = value } }, Cmd.none )

        GenderChanged value ->
            ( { model | data = { data | gender = value } }, Cmd.none )

        StatusChanged value ->
            ( { model | data = { data | status = value } }, Cmd.none )

        AdminChanged value ->
            ( { model | data = { data | admin = value } }, Cmd.none )

        PasswordChanged value ->
            ( { model | data = { data | password = Just value } }, Cmd.none )

        PickerChanged which event ->
            case event of
                DatePicker.PickerChanged change ->
                    ( { model
                        | formState =
                            model.formState
                                |> updateFormStatePicker which (DatePicker.update change)
                      }
                    , Cmd.none
                    )

                DatePicker.DateChanged date ->
                    ( { model
                        | data = data |> updateTutor which date
                        , formState = model.formState |> updateFormStateText which (date |> Date.toIsoString)
                      }
                    , Cmd.none
                    )

                DatePicker.TextChanged text ->
                    let
                        date =
                            Date.fromIsoString text |> Result.withDefault (getTutorField which data)
                    in
                    ( { model
                        | formState =
                            model.formState
                                |> updateFormStateText which text
                                |> updateFormStatePicker which (DatePicker.setVisibleMonth date)
                        , data = data |> updateTutor which date
                      }
                    , Cmd.none
                    )

        SetToday today ->
            ( { model
                | formState =
                    model.formState
                        |> updateFormStatePicker Dob (DatePicker.setToday today)
                        |> updateFormStatePicker Doc (DatePicker.setToday today)
              }
            , Cmd.none
            )

        Submit ->
            if validateAll model.data model.formState then
                case model.id of
                    Nothing ->
                        -- new tutor
                        ( model, postNewTutor model.credentials model.data )

                    Just tutorId ->
                        -- update existing
                        ( model, postTutorUpdate model.credentials model.data )

            else
                ( { model | errorMessage = Just "Some fields are invalid." }, Cmd.none )

        GotUpdated result ->
            case result of
                Ok _ ->
                    ( { model | successMessage = Just "Updated successfully." }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (Api.errorToString error) }, Cmd.none )

        GotTutorAdded result ->
            case result of
                Ok tutorId ->
                    ( { model | id = Just tutorId, successMessage = Just "Updated successfully." }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (Api.errorToString error) }, Cmd.none )

        ToProfile ->
            case model.id of
                Nothing ->
                    ( model, Navigation.back model.key 1 )

                Just id ->
                    ( model, Navigation.pushUrl model.key (Builder.absolute [ "tutor", id ] []) )


validateAll : Tutor -> FormState -> Bool
validateAll tutor form =
    List.all Basics.identity
        [ String.isEmpty tutor.name |> not
        , String.isEmpty tutor.school |> not
        , String.isEmpty tutor.email |> not
        , Date.toIsoString tutor.dateOfBirth == form.dobText
        , Date.toIsoString tutor.dateOfRegistration == form.docText
        ]


viewValidation : Bool -> Element Msg
viewValidation validated =
    let
        color =
            if validated then
                Colors.green

            else
                Colors.red
    in
    Element.el
        [ Element.height Element.fill
        , Element.width (Element.px 20)
        , Element.padding 5
        ]
        (Element.el
            [ Background.color color
            , Element.height Element.fill
            , Element.width Element.fill
            , Border.rounded 50
            ]
            Element.none
        )


viewRow : String -> Tutor -> (Tutor -> String) -> (String -> Msg) -> (String -> Bool) -> Element Msg
viewRow label data accessor updateMsg validator =
    let
        textFieldStyles =
            [ Element.padding 4
            , Element.width <| Element.px 200
            ]
    in
    Element.wrappedRow
        []
        [ Input.text textFieldStyles
            { label = Element.text label |> Input.labelLeft [ Element.width (Element.px 150) ]
            , onChange = updateMsg
            , placeholder = Nothing
            , text = accessor data
            }
        , viewValidation (validator (accessor data))
        ]


viewRowPassword : String -> Tutor -> (Tutor -> Maybe String) -> (String -> Msg) -> (Maybe String -> Bool) -> Element Msg
viewRowPassword label data accessor updateMsg validator =
    let
        textFieldStyles =
            [ Element.padding 4
            , Element.width <| Element.px 200
            ]
    in
    Element.wrappedRow
        []
        [ Input.newPassword textFieldStyles
            { label = Element.text label |> Input.labelLeft [ Element.width (Element.px 150) ]
            , onChange = updateMsg
            , placeholder = Nothing
            , text = accessor data |> withDefault ""
            , show = False
            }
        , viewValidation (validator (accessor data))
        ]


viewRowDatePicker : String -> Date.Date -> String -> DatePicker.Model -> (DatePicker.ChangeEvent -> Msg) -> Element Msg
viewRowDatePicker label selectedDate dateText pickerModel changePicker =
    let
        dateFieldStyles =
            [ Element.padding 4
            , Element.width <| Element.px 100
            ]
    in
    Element.row []
        [ DatePicker.input
            dateFieldStyles
            { onChange = changePicker
            , selected = Just selectedDate
            , label = Element.text label |> Input.labelLeft [ Element.width (Element.px 150) ]
            , placeholder = Maybe.Nothing
            , settings = DatePicker.defaultSettings
            , text = dateText
            , model = pickerModel
            }
        , viewValidation (Date.toIsoString selectedDate == dateText)
        ]


viewRowChoice : String -> List ( a, String ) -> (a -> Msg) -> a -> Element Msg
viewRowChoice label options msg choice =
    let
        activeGreen =
            Element.rgb 0 255 0

        inactiveWhite =
            Element.rgb 255 255 255

        backgroundColor : Bool -> Element.Color
        backgroundColor active =
            if active then
                activeGreen

            else
                inactiveWhite

        fontColor : Bool -> Element.Color
        fontColor active =
            Colors.black
    in
    Element.row
        [ Element.spacing 5 ]
        ((Element.text
            label
            |> Element.el [ Element.width (Element.px 150) ]
         )
            :: List.map
                (\( op, oplabel ) ->
                    Input.button
                        [ Background.color (backgroundColor (op == choice))
                        , Font.color (fontColor (op == choice))
                        , Element.paddingXY 5 2
                        , Border.rounded 3
                        , Border.width 1
                        ]
                        { label = Element.text oplabel, onPress = Just (msg op) }
                )
                options
        )


viewForm : Bool -> Tutor -> FormState -> Element Msg
viewForm isNew data form =
    Element.column
        [ Element.spacing 10 ]
        [ viewRow "Name" data .name NameChanged (String.isEmpty >> Basics.not)
        , viewRow "School" data .school SchoolChanged (String.isEmpty >> Basics.not)
        , viewRow "Email" data .email EmailChanged isValidEmail
        , viewRowDatePicker
            "Date of Birth"
            data.dateOfBirth
            form.dobText
            form.dobPicker
            (PickerChanged Dob)
        , viewRowChoice "Gender" [ ( Tutor.Male, "Male" ), ( Tutor.Female, "Female" ) ] GenderChanged data.gender
        , viewRowChoice "Admin Level" [ ( Tutor.LvlAdmin, "Admin" ), ( Tutor.LvlTutor, "Tutor" ) ] AdminChanged data.admin
        , viewRowChoice "Tutor Status" [ ( Tutor.Active, "Active" ), ( Tutor.Inactive, "Inactive" ), ( Tutor.New, "New" ) ] StatusChanged data.status
        , viewRowDatePicker
            "Start from"
            data.dateOfRegistration
            form.docText
            form.docPicker
            (PickerChanged Doc)
        , viewRowPassword "Password" data .password PasswordChanged (isValidPassword isNew)
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.padding 20
        , Element.width Element.fill
        , Element.height Element.fill
        , Background.color Colors.theme.p50
        , Element.spacing 20
        ]
        [ viewForm (Maybe.Extra.isNothing model.id) model.data model.formState
        , Input.button
            [ Background.color Colors.theme.a400
            , Border.width 1
            , Border.rounded 3
            , Element.paddingXY 10 2
            , Element.mouseOver [ Background.color Colors.theme.a200 ]
            ]
            { onPress = Just Submit
            , label = Element.text "Submit"
            }
        , case model.errorMessage of
            Nothing ->
                Element.none

            Just message ->
                Element.text message |> Element.el [ Font.color Colors.red, Font.bold ]
        , case model.successMessage of
            Nothing ->
                Element.none

            Just message ->
                Element.row [ Element.spacing 30 ]
                    [ Element.text message |> Element.el [ Font.color Colors.theme.p600, Font.bold ]
                    , Input.button Styles.buttonStyleWide
                        { onPress = Just ToProfile
                        , label = Element.text "Back to profile" |> Element.el [ Element.centerX ]
                        }
                    ]
        ]
