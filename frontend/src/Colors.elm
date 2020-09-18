module Colors exposing (black, clear, grey, theme, white)

import Element exposing (Color)
import Hex


type alias ColorScheme =
    { p50 : Color
    , p100 : Color
    , p200 : Color
    , p300 : Color
    , p400 : Color
    , p500 : Color
    , p600 : Color
    , p700 : Color
    , p800 : Color
    , p900 : Color
    , a100 : Color
    , a200 : Color
    , a400 : Color
    , a700 : Color
    }


hexStringToColor : String -> Color
hexStringToColor hexString =
    let
        r =
            String.left 2 hexString |> Hex.fromString |> Result.withDefault 0

        g =
            String.left 2 (String.dropLeft 2 hexString) |> Hex.fromString |> Result.withDefault 0

        b =
            String.left 2 (String.dropLeft 4 hexString) |> Hex.fromString |> Result.withDefault 0
    in
    Element.rgb255 r g b


clear : Color
clear =
    Element.rgba255 0 0 0 0


white : Color
white =
    Element.rgb255 200 200 200


black : Color
black =
    Element.rgb255 30 30 30


grey : Color
grey =
    Element.rgb255 150 150 150


theme : ColorScheme
theme =
    { p50 = hexStringToColor "f9fbe7"
    , p100 = hexStringToColor "f0f4c3"
    , p200 = hexStringToColor "e6ee9c"
    , p300 = hexStringToColor "dce775"
    , p400 = hexStringToColor "d4e157"
    , p500 = hexStringToColor "cddc39"
    , p600 = hexStringToColor "c0ca33"
    , p700 = hexStringToColor "afb42b"
    , p800 = hexStringToColor "9e9d24"
    , p900 = hexStringToColor "827717"
    , a100 = hexStringToColor "f4ff81"
    , a200 = hexStringToColor "eeff41"
    , a400 = hexStringToColor "c6ff00"
    , a700 = hexStringToColor "aeea00"
    }
