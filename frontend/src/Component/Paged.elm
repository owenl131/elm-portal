module Component.Paged exposing
    ( Msg(..)
    , Paged
    , pagedDecoder
    , viewPagination
    )

import Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type alias Paged a =
    { page : Int
    , perPage : Int
    , total : Int
    , data : a
    }


type Msg
    = ChangePagePrevious
    | ChangePageNext
    | ChangePage Int


computeLastPage : Paged a -> Int
computeLastPage paged =
    if remainderBy paged.total paged.perPage == 0 then
        paged.total // paged.perPage

    else
        (paged.total // paged.perPage) + 1


pagedDecoder : Decode.Decoder a -> Decode.Decoder (Paged a)
pagedDecoder subDecoder =
    Decode.succeed Paged
        |> Pipeline.required "page" Decode.int
        |> Pipeline.required "perPage" Decode.int
        |> Pipeline.required "total" Decode.int
        |> Pipeline.required "data" subDecoder


viewPagination : Paged a -> Element Msg
viewPagination pagedData =
    Element.row
        [ Element.width Element.fill
        , Background.color Colors.theme.p50
        , Element.spacing 20
        , Element.padding 10
        ]
        (Element.el [ Element.alignLeft ]
            (Element.text (String.fromInt pagedData.total ++ " entries found"))
            :: (if pagedData.page == 0 then
                    Element.el [ Element.centerX, Font.color Colors.grey ] (Element.text "<")

                else
                    Input.button
                        [ Element.centerX ]
                        { onPress = Just ChangePagePrevious, label = Element.text "<" }
               )
            :: List.map
                (\p ->
                    Input.button []
                        { onPress =
                            if pagedData.page == p - 1 then
                                Nothing

                            else
                                Just (ChangePage (p - 1))
                        , label =
                            Element.text (String.fromInt p)
                                |> Element.el
                                    (if pagedData.page == p - 1 then
                                        [ Font.bold ]

                                     else
                                        []
                                    )
                        }
                )
                (List.range 1 (computeLastPage pagedData + 1))
            ++ [ if pagedData.page == computeLastPage pagedData then
                    Element.el [ Element.centerX, Font.color Colors.grey ] (Element.text ">")

                 else
                    Input.button [ Element.centerX ] { onPress = Just ChangePageNext, label = Element.text ">" }
               , Element.el [ Element.alignLeft, Font.color Colors.clear ]
                    (Element.text (String.fromInt pagedData.total ++ " entries found"))
               ]
        )
