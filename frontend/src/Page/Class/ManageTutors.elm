module Page.Class.ManageTutors exposing
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Maybe.Extra exposing (join)
import RemoteData exposing (WebData, withDefault)
import Styles
import Task
import Tutor exposing (Tutor)
import Url.Builder as Builder
import Utils


type alias Modal =
    { msg : Msg
    , title : String
    , description : String
    }


type alias Model =
    { key : Navigation.Key
    , credentials : Api.Credentials
    , id : Class.ClassId
    , tutors : WebData (List ClassTutor)
    , today : Date.Date
    , nameFilter : String
    , classData : WebData Class.Class
    , joinDate : Maybe Date.Date
    , joinDatePicker : DatePicker.Model
    , suggestions : WebData (List TutorSuggestion)
    , modal : Maybe Modal
    , hoveredIndex : Int
    , editForm : Maybe EditForm
    }


type alias EditForm =
    { joinDate : Maybe Date.Date
    , joinDateText : String
    , joinDatePicker : DatePicker.Model
    , leaveDate : Maybe Date.Date
    , leaveDateText : String
    , leaveDatePicker : DatePicker.Model
    , tutorId : Tutor.TutorId
    , name : String
    }


type Msg
    = GotTutorSuggestionList (Result Http.Error (List TutorSuggestion))
    | GotTutorList (Result Http.Error (List ClassTutor))
    | GotClassData (Result Http.Error Class.Class)
    | AddTutor Tutor.TutorId
    | GotAddTutorResult (Result Http.Error ())
    | GotUpdateTutorResult (Result Http.Error ())
    | GotRemovedTutorResult (Result Http.Error ())
    | EnteredNameFilter String
    | EditDate Tutor.TutorId
    | EditJoinDateChanged DatePicker.ChangeEvent
    | EditLeaveDateChanged DatePicker.ChangeEvent
    | CancelEditForm
    | SubmitEditForm
    | FetchSuggestions
    | PickerChanged DatePicker.ChangeEvent
    | SetToday Date.Date
    | ShowModal Msg String String
    | ModalCancel
    | HoverChanged Int
    | PostRemoveTutor Tutor.TutorId


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
    "Manage Tutors"


getPageLink : Model -> String
getPageLink model =
    Builder.absolute [ "class", model.id, "managetutors" ] []


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
            , modal = Nothing
            , hoveredIndex = -1
            , editForm = Nothing
            , today = Date.fromRataDie 1
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


postUpdateDates : Api.Credentials -> Class.ClassId -> Tutor.TutorId -> Date.Date -> Maybe Date.Date -> Cmd Msg
postUpdateDates credentials classId tutorId joinDate leaveDate =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session) ]
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "updatetutor", tutorId ] []
        , body =
            Http.jsonBody
                (Encode.object
                    ((case leaveDate of
                        Just date ->
                            [ ( "leaveDate", Encode.string (Date.toIsoString date) ) ]

                        Nothing ->
                            []
                     )
                        ++ [ ( "joinDate", Encode.string (Date.toIsoString joinDate) )
                           ]
                    )
                )
        , expect = Http.expectWhatever GotUpdateTutorResult
        }


postAddTutor : Api.Credentials -> Class.ClassId -> Tutor.TutorId -> Date.Date -> Cmd Msg
postAddTutor credentials classId tutorId joinDate =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session)
            ]
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


postRemoveTutor : Api.Credentials -> Class.ClassId -> Tutor.TutorId -> Cmd Msg
postRemoveTutor credentials classId tutorId =
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ Base64.encode credentials.session)
            ]
        , timeout = Nothing
        , tracker = Nothing
        , url = Builder.crossOrigin Api.endpoint [ "class", classId, "removetutor", tutorId ] []
        , body = Http.emptyBody
        , expect = Http.expectWhatever GotRemovedTutorResult
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
                , today = date
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

        GotRemovedTutorResult _ ->
            ( model, fetchTutorList model.credentials model.id )

        GotUpdateTutorResult _ ->
            ( { model | editForm = Nothing }, fetchTutorList model.credentials model.id )

        CancelEditForm ->
            ( { model | editForm = Nothing }, Cmd.none )

        SubmitEditForm ->
            case model.editForm of
                Nothing ->
                    ( model, Cmd.none )

                Just form ->
                    case form.joinDate of
                        Nothing ->
                            ( model, Cmd.none )

                        Just joinDate ->
                            ( model, postUpdateDates model.credentials model.id form.tutorId joinDate form.leaveDate )

        EditDate tutorId ->
            let
                form : Maybe EditForm
                form =
                    model.tutors
                        |> RemoteData.toMaybe
                        |> Maybe.map (List.filter (.id >> (==) tutorId))
                        |> Maybe.withDefault []
                        |> List.head
                        |> Maybe.map
                            (\t ->
                                { joinDate = Just t.joinDate
                                , joinDateText = t.joinDate |> Date.toIsoString
                                , joinDatePicker = DatePicker.initWithToday model.today |> DatePicker.setVisibleMonth t.joinDate
                                , leaveDate = t.leaveDate
                                , leaveDateText = t.leaveDate |> Maybe.map Date.toIsoString |> Maybe.withDefault ""
                                , leaveDatePicker = DatePicker.initWithToday model.today |> DatePicker.setVisibleMonth (t.leaveDate |> Maybe.withDefault model.today)
                                , tutorId = t.id
                                , name = t.name
                                }
                            )
            in
            ( { model | editForm = form }, Cmd.none )

        AddTutor tutorId ->
            case model.joinDate of
                Nothing ->
                    ignore

                Just joinDate ->
                    ( model, postAddTutor model.credentials model.id tutorId joinDate )

        PostRemoveTutor tutorId ->
            ( model, postRemoveTutor model.credentials model.id tutorId )

        ShowModal modalMsg title description ->
            ( { model | modal = Just { msg = modalMsg, title = title, description = description } }, Cmd.none )

        ModalCancel ->
            ( { model | modal = Nothing }, Cmd.none )

        HoverChanged value ->
            ( { model | hoveredIndex = value }, Cmd.none )

        EditJoinDateChanged changeEvent ->
            case model.editForm of
                Nothing ->
                    ( model, Cmd.none )

                Just form ->
                    case changeEvent of
                        DatePicker.DateChanged date ->
                            ( { model
                                | editForm =
                                    Just
                                        { form
                                            | joinDate = Just date
                                            , joinDateText = Date.toIsoString date
                                            , joinDatePicker = form.joinDatePicker |> DatePicker.setVisibleMonth date
                                        }
                              }
                            , Cmd.none
                            )

                        DatePicker.TextChanged text ->
                            let
                                date =
                                    Date.fromIsoString text |> Result.toMaybe
                            in
                            ( { model
                                | editForm =
                                    Just
                                        { form
                                            | joinDate = date
                                            , joinDateText = text
                                            , joinDatePicker = form.joinDatePicker |> (date |> Maybe.map DatePicker.setVisibleMonth |> Maybe.withDefault Basics.identity)
                                        }
                              }
                            , Cmd.none
                            )

                        DatePicker.PickerChanged subMsg ->
                            ( { model | editForm = Just { form | joinDatePicker = DatePicker.update subMsg form.joinDatePicker } }, Cmd.none )

        EditLeaveDateChanged changeEvent ->
            case model.editForm of
                Nothing ->
                    ( model, Cmd.none )

                Just form ->
                    case changeEvent of
                        DatePicker.DateChanged date ->
                            ( { model
                                | editForm =
                                    Just
                                        { form
                                            | leaveDate = Just date
                                            , leaveDateText = Date.toIsoString date
                                            , leaveDatePicker = form.leaveDatePicker |> DatePicker.setVisibleMonth date
                                        }
                              }
                            , Cmd.none
                            )

                        DatePicker.TextChanged text ->
                            let
                                date =
                                    Date.fromIsoString text |> Result.toMaybe
                            in
                            ( { model
                                | editForm =
                                    Just
                                        { form
                                            | leaveDate = date
                                            , leaveDateText = text
                                            , leaveDatePicker = form.leaveDatePicker |> (date |> Maybe.map DatePicker.setVisibleMonth |> Maybe.withDefault Basics.identity)
                                        }
                              }
                            , Cmd.none
                            )

                        DatePicker.PickerChanged subMsg ->
                            ( { model | editForm = Just { form | leaveDatePicker = DatePicker.update subMsg form.leaveDatePicker } }, Cmd.none )

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
        toHeader =
            Utils.toHeader
    in
    -- for each suggestion, display name, admin, and button to add
    Element.table
        [ Element.spacing 5
        , Element.paddingXY 0 10
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


viewModal : Modal -> Element Msg
viewModal modal =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (Element.rgba255 0 0 0 0.2)
        , Events.onClick ModalCancel
        ]
        (Element.column
            [ Background.color Colors.white
            , Element.spacing 10
            , Element.padding 20
            , Element.centerX
            , Element.centerY
            , Border.shadow { offset = ( 1, 1 ), size = 2, blur = 5, color = Colors.black }
            ]
            [ Element.text modal.title |> Element.el [ Font.bold ]
            , Element.paragraph
                [ Element.width (Element.fill |> Element.maximum 200)
                ]
                [ Element.text modal.description ]
            , Element.row [ Element.spacing 5 ]
                [ Input.button
                    [ Element.paddingXY 20 5
                    , Border.width 1
                    , Border.rounded 5
                    ]
                    { label = Element.text "Cancel"
                    , onPress = Just ModalCancel
                    }
                , Input.button
                    [ Element.paddingXY 20 5
                    , Border.width 1
                    , Background.color Colors.red
                    , Font.color Colors.white
                    , Border.rounded 5
                    ]
                    { label = Element.text "Proceed"
                    , onPress = Just modal.msg
                    }
                ]
            ]
        )


viewSelector : Model -> Element Msg
viewSelector model =
    Element.column
        [ Element.width Element.fill
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
            Styles.buttonStyleCozy
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
        , Utils.viewWebData viewSuggestions model.suggestions
        ]


viewList : Int -> List ClassTutor -> Element Msg
viewList hovered tutors =
    let
        toHeader =
            Utils.toHeader

        cell =
            Utils.cell HoverChanged Nothing hovered
    in
    Element.indexedTable
        [ Element.padding 20
        , Border.color Colors.theme.p50
        , Border.width 3
        , Element.width Element.fill
        ]
        { data = tutors
        , columns =
            [ { header = "Name" |> toHeader
              , width = Element.fill |> Element.maximum 150 |> Element.minimum 100
              , view = .name >> Element.text |> cell
              }
            , { header = "Joined Class on" |> toHeader
              , width = Element.fill |> Element.maximum 100
              , view = .joinDate >> Date.toIsoString >> Element.text |> cell
              }
            , { header = "Left Class on" |> toHeader
              , width = Element.fill |> Element.maximum 100
              , view = .leaveDate >> Maybe.map Date.toIsoString >> Maybe.withDefault "<Not set>" >> Element.text |> cell
              }
            , { header = "Edit Dates" |> toHeader
              , width = Element.fill |> Element.maximum 90
              , view =
                    (\t ->
                        Input.button
                            Styles.buttonStyleCozy
                            { onPress = Just (EditDate t.id)
                            , label =
                                "Edit"
                                    |> Element.text
                                    |> Element.el [ Element.centerX ]
                            }
                    )
                        |> cell
              }
            , { header = "Delete" |> toHeader
              , width = Element.fill |> Element.maximum 75
              , view =
                    (\t ->
                        Input.button
                            [ Background.color Colors.red
                            , Font.color Colors.white
                            , Border.width 1
                            , Border.rounded 3
                            , Element.paddingXY 10 4
                            ]
                            { onPress = Just (ShowModal (PostRemoveTutor t.id) "Remove tutor from class?" "Attendance records will be lost. Use only if tutor is added to the class wrongly.")
                            , label = Element.text "Delete" |> Element.el [ Element.centerX ]
                            }
                    )
                        |> cell
              }
            ]
        }


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


viewEditDatesForm : EditForm -> Element Msg
viewEditDatesForm form =
    let
        joinDateValid =
            Maybe.Extra.isJust form.joinDate
                && ((form.joinDate |> Maybe.map Date.toIsoString |> Maybe.withDefault "")
                        == form.joinDateText
                   )

        leaveDateValid =
            ((form.leaveDate |> Maybe.map Date.toIsoString |> Maybe.withDefault "") == form.leaveDateText)
                && (case form.joinDate of
                        Nothing ->
                            Basics.True

                        Just joinDate ->
                            case form.leaveDate of
                                Nothing ->
                                    Basics.True

                                Just leaveDate ->
                                    Date.toRataDie joinDate <= Date.toRataDie leaveDate
                   )
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.spacing 10
        , Background.color Colors.theme.p50
        ]
        [ "Edit for " ++ form.name |> Element.text |> Element.el [ Font.bold ]
        , Element.el [ Element.height (Element.px 10) ] Element.none
        , Element.row []
            [ DatePicker.input
                Styles.dateFieldStyle
                { label = "Join Date" |> Element.text |> Input.labelLeft Styles.textLabelStyle
                , model = form.joinDatePicker
                , placeholder = Nothing
                , selected = form.joinDate
                , settings = DatePicker.defaultSettings
                , text = form.joinDateText
                , onChange = EditJoinDateChanged
                }
            , viewValidation joinDateValid
            ]
        , Element.row []
            [ DatePicker.input
                Styles.dateFieldStyle
                { label = "Leave Date" |> Element.text |> Input.labelLeft Styles.textLabelStyle
                , model = form.leaveDatePicker
                , placeholder = Nothing
                , selected = form.leaveDate
                , settings = DatePicker.defaultSettings
                , text = form.leaveDateText
                , onChange = EditLeaveDateChanged
                }
            , viewValidation leaveDateValid
            ]
        , case form.joinDate of
            Nothing ->
                Element.text "Join Date must be set" |> Element.el [ Font.color Colors.red, Element.paddingXY 0 5 ]

            Just joinDate ->
                case form.leaveDate of
                    Nothing ->
                        Element.none

                    Just leaveDate ->
                        if Date.toRataDie joinDate > Date.toRataDie leaveDate then
                            Element.text "Join Date must be earlier than Leave Date" |> Element.el [ Font.color Colors.red, Element.paddingXY 0 5 ]

                        else
                            Element.none
        , Element.row [ Element.spacing 10, Element.paddingXY 0 5 ]
            [ Input.button Styles.buttonStyleCozyWhite { label = "Cancel" |> Element.text, onPress = Just CancelEditForm }
            , if joinDateValid && leaveDateValid then
                Input.button Styles.buttonStyleCozy
                    { label = "Update" |> Element.text
                    , onPress = Just SubmitEditForm
                    }

              else
                Element.none
            ]
        ]


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 10
        , Element.padding 20
        , Element.inFront (model.modal |> Maybe.map viewModal |> Maybe.withDefault Element.none)
        ]
        [ Utils.viewWebData (viewList model.hoveredIndex) model.tutors
        , model.editForm |> Maybe.map viewEditDatesForm |> Maybe.withDefault Element.none
        , viewSelector model
        ]
