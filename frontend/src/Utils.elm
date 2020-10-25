module Utils exposing (..)

import Api
import Element exposing (Element)
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
