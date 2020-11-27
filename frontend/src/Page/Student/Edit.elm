module Page.Student.Edit exposing (..)

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
import Maybe.Extra
import Regex
import Student exposing (Student, StudentId, StudentStatus, emptyStudent)
import Styles
import Task
import Url.Builder as Builder
import Utils
import Validate


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Maybe StudentId
    , data : Student
    , formState : FormState
    , errorMessage : Maybe String
    , successMessage : Maybe String
    }


type alias FormState =
    { dobPicker : DatePicker.Model
    , dobText : String
    , docPicker : DatePicker.Model
    , docText : String
    }


type WhichFormElement
    = Dob
    | Doc


type Msg
    = NameChanged String
    | GenderChanged Utils.Gender
    | StatusChanged StudentStatus
    | PickerChanged WhichFormElement DatePicker.ChangeEvent
    | SetToday Date.Date
    | GotStudentData (Result Http.Error Student)
    | GotUpdated (Result Http.Error ())
    | GotStudentAdded (Result Http.Error String)
    | ToProfile
    | Submit


getPageTitle : Model -> String
getPageTitle model =
    case model.id of
        Nothing ->
            "New Student"

        Just _ ->
            "Edit: " ++ model.data.name


getPageLink : Model -> String
getPageLink model =
    case model.id of
        Nothing ->
            Builder.absolute [ "students", "new" ] []

        Just studentId ->
            Builder.absolute [ "student", studentId, "edit" ] []


postNewStudent : Api.Credentials -> Student -> Cmd Msg
postNewStudent credentials student =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Student.studentEncoder student)
        , url = Builder.crossOrigin Api.endpoint [ "students", "new" ] []
        , expect = Http.expectJson GotStudentAdded (Decode.field "id" Decode.string)
        , timeout = Nothing
        , tracker = Nothing
        }


postStudentUpdate : Api.Credentials -> Student -> Cmd Msg
postStudentUpdate credentials student =
    Http.request
        { method = "PATCH"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.jsonBody (Student.studentEncoder student)
        , url = Builder.crossOrigin Api.endpoint [ "student", student.id ] []
        , expect = Http.expectWhatever GotUpdated
        , timeout = Nothing
        , tracker = Nothing
        }


fetchStudentData : Api.Credentials -> StudentId -> Cmd Msg
fetchStudentData credentials id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , body = Http.emptyBody
        , url = Builder.crossOrigin Api.endpoint [ "student", id ] []
        , expect = Http.expectJson GotStudentData Student.studentDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


initWithEmpty : Api.Credentials -> Navigation.Key -> ( Model, Cmd Msg )
initWithEmpty credentials key =
    ( { key = key
      , credentials = credentials
      , id = Nothing
      , data = Student.emptyStudent
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


initWithStudent : Api.Credentials -> Navigation.Key -> String -> ( Model, Cmd Msg )
initWithStudent credentials key id =
    ( { key = key
      , credentials = credentials
      , id = Just id
      , data = Student.emptyStudent
      , formState =
            { dobPicker = DatePicker.init
            , docPicker = DatePicker.init
            , dobText = ""
            , docText = ""
            }
      , errorMessage = Nothing
      , successMessage = Nothing
      }
    , fetchStudentData credentials id
    )


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


updateStudent : WhichFormElement -> Date.Date -> Student -> Student
updateStudent which date student =
    case which of
        Dob ->
            { student | dateOfBirth = date }

        Doc ->
            { student | dateOfRegistration = date }


getStudentField : WhichFormElement -> Student -> Date.Date
getStudentField which student =
    case which of
        Dob ->
            student.dateOfBirth

        Doc ->
            student.dateOfRegistration


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
    case msg of
        GotStudentData result ->
            case Result.toMaybe result of
                Nothing ->
                    ( { model | data = emptyStudent }, Cmd.none )

                Just student ->
                    ( { model
                        | data = student
                        , formState =
                            model.formState
                                |> updateFormStateText Dob (Date.toIsoString student.dateOfBirth)
                                |> updateFormStateText Doc (Date.toIsoString student.dateOfRegistration)
                                |> updateFormStatePicker Dob (DatePicker.setVisibleMonth student.dateOfBirth)
                                |> updateFormStatePicker Doc (DatePicker.setVisibleMonth student.dateOfRegistration)
                      }
                    , Cmd.none
                    )

        NameChanged value ->
            ( { model | data = { data | name = value } }, Cmd.none )

        GenderChanged value ->
            ( { model | data = { data | gender = value } }, Cmd.none )

        StatusChanged value ->
            ( { model | data = { data | status = value } }, Cmd.none )

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
                        | data = data |> updateStudent which date
                        , formState =
                            model.formState
                                |> updateFormStateText which (date |> Date.toIsoString)
                      }
                    , Cmd.none
                    )

                DatePicker.TextChanged text ->
                    let
                        date =
                            Date.fromIsoString text
                                |> Result.withDefault (getStudentField which data)
                    in
                    ( { model
                        | formState =
                            model.formState
                                |> updateFormStateText which text
                                |> updateFormStatePicker which (DatePicker.setVisibleMonth date)
                        , data = data |> updateStudent which date
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
                        -- new student
                        ( model, postNewStudent model.credentials model.data )

                    Just _ ->
                        -- update existing
                        ( model, postStudentUpdate model.credentials model.data )

            else
                ( { model | errorMessage = Just "Some fields are invalid." }, Cmd.none )

        GotUpdated result ->
            case result of
                Ok _ ->
                    ( { model | successMessage = Just "Updated successfully." }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (Api.errorToString error) }, Cmd.none )

        GotStudentAdded result ->
            case result of
                Ok id ->
                    ( { model | id = Just id, successMessage = Just "Added successfully." }, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (Api.errorToString error) }, Cmd.none )

        ToProfile ->
            case model.id of
                Nothing ->
                    ( model, Navigation.back model.key 1 )

                Just id ->
                    ( model, Navigation.pushUrl model.key (Builder.absolute [ "student", id ] []) )


validateAll : Student -> FormState -> Bool
validateAll student form =
    True


viewRow : String -> Student -> (Student -> String) -> (String -> Msg) -> (String -> Bool) -> Element Msg
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
        , Utils.viewValidation (validator (accessor data))
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
        , Utils.viewValidation (Date.toIsoString selectedDate == dateText)
        ]


viewRowChoice : String -> List ( a, String ) -> (a -> Msg) -> a -> Element Msg
viewRowChoice label options msg choice =
    let
        activeGreen =
            Element.rgb 0 255 0

        inactiveWhite =
            Colors.white

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


viewForm : Bool -> Student -> FormState -> Element Msg
viewForm isNew data form =
    Element.column
        [ Element.spacing 10 ]
        [ viewRow "Name" data .name NameChanged (String.isEmpty >> Basics.not)
        , viewRowDatePicker
            "Date of Birth"
            data.dateOfBirth
            form.dobText
            form.dobPicker
            (PickerChanged Dob)
        , viewRowChoice "Gender" [ ( Utils.Male, "Male" ), ( Utils.Female, "Female" ) ] GenderChanged data.gender
        , viewRowChoice "Student Status" [ ( Student.Active, "Active" ), ( Student.Inactive, "Inactive" ) ] StatusChanged data.status
        , viewRowDatePicker
            "Joined on"
            data.dateOfRegistration
            form.docText
            form.docPicker
            (PickerChanged Doc)
        ]


view : Model -> Element Msg
view model =
    Element.el
        [ Element.padding 20
        , Element.width Element.fill
        ]
        (Element.column
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
        )
