module Utils exposing (..)

import Api
import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Maybe.Extra
import RemoteData exposing (WebData)
import Time


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
    text |> Element.text |> Element.el [ Font.bold, Element.paddingEach { top = 0, bottom = 8, left = 5, right = 5 } ]


cell : (Int -> msg) -> Maybe (elem -> msg) -> Int -> (elem -> Element msg) -> Int -> elem -> Element msg
cell hoverChanged redirect hovered toElem index e =
    Element.el
        ([ Element.centerY
         , Events.onMouseEnter (hoverChanged index)
         , Events.onMouseLeave (hoverChanged -1)
         , Element.height Element.fill
         , Element.padding 4
         , Background.color (ifElse Colors.theme.p100 Colors.clear (index == hovered))
         ]
            ++ (redirect |> Maybe.Extra.toList |> List.map (\r -> r e) |> List.map Events.onDoubleClick)
        )
        (toElem e |> Element.el [ Element.centerY ])
