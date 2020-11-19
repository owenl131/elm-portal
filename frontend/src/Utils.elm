module Utils exposing (..)

import Api
import Colors
import Date
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Json.Decode as Decode
import Maybe.Extra
import RemoteData exposing (WebData)
import Time


dateDecoder : Decode.Decoder Date.Date
dateDecoder =
    Decode.string
        |> Decode.map Date.fromIsoString
        |> Decode.andThen
            (\result ->
                case result of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        Decode.fail error
            )


type Gender
    = Male
    | Female


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"


toGender : String -> Maybe Gender
toGender gender =
    case String.toLower gender of
        "m" ->
            Just Male

        "f" ->
            Just Female

        "male" ->
            Just Male

        "female" ->
            Just Female

        _ ->
            Nothing


genderDecoder : Decode.Decoder Gender
genderDecoder =
    Decode.string
        |> Decode.map toGender
        |> Decode.map (Maybe.map Decode.succeed)
        |> Decode.andThen
            (Maybe.withDefault (Decode.fail "Invalid gender"))


genderEncoder : Gender -> String
genderEncoder gender =
    case gender of
        Male ->
            "m"

        Female ->
            "f"


allDays : List Time.Weekday
allDays =
    [ Time.Mon
    , Time.Tue
    , Time.Wed
    , Time.Thu
    , Time.Fri
    , Time.Sat
    , Time.Sun
    ]


daysToString : Time.Weekday -> String
daysToString day =
    case day of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tues"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thur"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


viewWebData : (a -> Element msg) -> WebData a -> Element msg
viewWebData viewFn webdata =
    case webdata of
        RemoteData.NotAsked ->
            Element.text "Not Asked"

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure err ->
            Element.text (Api.errorToString err)

        RemoteData.Success data ->
            viewFn data


ifElse : a -> a -> Bool -> a
ifElse resultIf resultElse result =
    if result then
        resultIf

    else
        resultElse


toHeader : String -> Element msg
toHeader text =
    text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, bottom = 8, left = 16, right = 16 } ]


cell : (Int -> msg) -> Maybe (elem -> msg) -> Int -> (elem -> Element msg) -> Int -> elem -> Element msg
cell hoverChanged redirect hovered toElem index e =
    Element.el
        ([ Element.centerY
         , Events.onMouseEnter (hoverChanged index)
         , Events.onMouseLeave (hoverChanged -1)
         , Element.height Element.fill
         , Element.paddingXY 16 4
         , Background.color (ifElse Colors.theme.p100 Colors.clear (index == hovered))
         ]
            ++ (redirect |> Maybe.Extra.toList |> List.map (\r -> r e) |> List.map Events.onDoubleClick)
        )
        (toElem e |> Element.el [ Element.centerY ])


cellCentered : (Int -> msg) -> Maybe (elem -> msg) -> Int -> (elem -> Element msg) -> Int -> elem -> Element msg
cellCentered hoverChanged redirect hovered toElem index e =
    Element.el
        ([ Element.centerY
         , Element.centerX
         , Events.onMouseEnter (hoverChanged index)
         , Events.onMouseLeave (hoverChanged -1)
         , Element.height Element.fill
         , Element.paddingXY 16 4
         , Background.color (ifElse Colors.theme.p100 Colors.clear (index == hovered))
         ]
            ++ (redirect |> Maybe.Extra.toList |> List.map (\r -> r e) |> List.map Events.onDoubleClick)
        )
        (toElem e |> Element.el [ Element.centerY ])


viewValidation : Bool -> Element msg
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
