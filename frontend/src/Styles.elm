module Styles exposing (..)

import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


buttonStyleComfy : List (Element.Attribute msg)
buttonStyleComfy =
    [ Background.color Colors.theme.a400
    , Border.width 1
    , Border.rounded 3
    , Element.paddingXY 10 4
    , Element.width (Element.shrink |> Element.minimum 100)
    , Element.mouseOver [ Background.color Colors.theme.a200 ]
    ]


buttonStyleCozy : List (Element.Attribute msg)
buttonStyleCozy =
    [ Background.color Colors.theme.a400
    , Border.width 1
    , Border.rounded 3
    , Element.paddingXY 10 4
    , Element.mouseOver [ Background.color Colors.theme.a200 ]
    ]


buttonStyleCozyRed : List (Element.Attribute msg)
buttonStyleCozyRed =
    [ Background.color Colors.red
    , Border.width 1
    , Border.rounded 3
    , Element.paddingXY 10 4
    ]


buttonStyleCozyWhite : List (Element.Attribute msg)
buttonStyleCozyWhite =
    [ Background.color Colors.white
    , Border.width 1
    , Border.rounded 3
    , Element.paddingXY 10 4
    ]


buttonStyleWide : List (Element.Attribute msg)
buttonStyleWide =
    [ Element.width (Element.px 120 |> Element.minimum 80)
    , Background.color Colors.theme.p200
    , Element.mouseOver [ Background.color Colors.theme.a200 ]
    , Border.width 1
    , Border.rounded 3
    , Element.paddingXY 20 10
    ]


dateFieldStyle : List (Element.Attribute msg)
dateFieldStyle =
    [ Element.padding 4
    , Element.width (Element.px 100)
    ]


textFieldStyle : List (Element.Attribute msg)
textFieldStyle =
    [ Element.padding 4
    , Element.width (Element.px 200)
    ]


textLabelStyle : List (Element.Attribute msg)
textLabelStyle =
    [ Element.width (Element.px 100), Element.centerY ]
