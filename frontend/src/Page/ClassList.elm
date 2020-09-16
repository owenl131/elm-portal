module Page.ClassList exposing (ClassFilters, Model, Msg, classFiltersFromUrl, init, update, view)

import Browser.Navigation as Navigation
import Class exposing (Class, classDecoder)
import Date
import Element exposing (Element)
import Element.Input as Input
import Http
import Json.Decode as Decode
import Maybe.Extra
import RemoteData exposing (WebData)
import Url.Builder as Builder
import Url.Parser.Query as Query


type alias Pagination =
    { page : Int
    , perPage : Int
    , lastPage : Int
    }


defaultPagination : Pagination
defaultPagination =
    { page = 0
    , perPage = 20
    , lastPage = 0
    }


type alias ClassFilters =
    { names : List String
    , yearLower : Maybe Int
    , yearUpper : Maybe Int
    , days : List Date.Weekday
    }


classFiltersFromUrl : Query.Parser ClassFilters
classFiltersFromUrl =
    Query.map4 ClassFilters
        (Query.custom "name" (\x -> x))
        (Query.int "yearLower")
        (Query.int "yearUpper")
        (Query.custom "days" (List.filterMap (String.toInt >> Maybe.map Date.numberToWeekday)))


classFiltersToQueryList : ClassFilters -> List Builder.QueryParameter
classFiltersToQueryList filters =
    List.map (Builder.int "yearLower") (Maybe.Extra.toList filters.yearLower)
        ++ List.map (Builder.int "yearUpper") (Maybe.Extra.toList filters.yearUpper)
        ++ List.map (Builder.string "name") filters.names
        ++ List.map (Date.weekdayToNumber >> Builder.int "days") filters.days


type alias Model =
    { key : Navigation.Key
    , pagination : Pagination
    , filters : ClassFilters
    , data : WebData (List Class)
    }


type Msg
    = ChangePagePrevious
    | ChangePageNext
    | ChangePage Int
    | GotClassList (Result Http.Error (List Class))
    | ToDetails Int


init : Navigation.Key -> ClassFilters -> ( Model, Cmd Msg )
init key filters =
    ( { key = key
      , pagination = defaultPagination
      , filters = filters
      , data = RemoteData.Loading
      }
    , Http.get
        { url = "http://localhost:5000/classes" ++ Builder.toQuery (classFiltersToQueryList filters)
        , expect = Http.expectJson GotClassList <| Decode.list classDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        ChangePageNext ->
            ignore

        ChangePagePrevious ->
            ignore

        ChangePage _ ->
            ignore

        GotClassList result ->
            case result of
                Ok data ->
                    ( { model | data = RemoteData.Success data }, Cmd.none )

                Err error ->
                    ( { model | data = RemoteData.Failure error }, Cmd.none )

        ToDetails id ->
            ( model, Navigation.pushUrl model.key ("/class/" ++ String.fromInt id) )


viewPagination : Pagination -> Element Msg
viewPagination _ =
    Element.row
        [ Element.width Element.fill ]
        [ Input.button [] { onPress = Just ChangePagePrevious, label = Element.text "<" }
        , Input.button [] { onPress = Just (ChangePage 1), label = Element.text "1" }
        , Input.button [] { onPress = Just ChangePageNext, label = Element.text ">" }
        ]


viewData : WebData (List Class) -> Element Msg
viewData data =
    case data of
        RemoteData.NotAsked ->
            Element.text "Not asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure error ->
            Element.text (Debug.toString error)

        RemoteData.Success classList ->
            Element.table
                []
                { columns =
                    [ { header = Element.text "Name"
                      , width = Element.fill
                      , view = .name >> Element.text
                      }
                    , { header = Element.text "Details"
                      , width = Element.fill
                      , view =
                            \class ->
                                Input.button
                                    []
                                    { label = Element.text "More"
                                    , onPress = Just (ToDetails class.id)
                                    }
                      }
                    ]
                , data = classList
                }


view : Model -> Element Msg
view model =
    Element.column
        []
        [ viewPagination model.pagination
        , viewData model.data
        , viewPagination model.pagination
        ]
