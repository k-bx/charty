module Charty.Labels exposing (LabelEntry, withLabels)

import Charty.Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias LabelEntry =
    ( Color, String )


type alias Config =
    { background : Color
    , labelsColor : Color
    }


withLabels : Config -> List LabelEntry -> Svg msg -> Svg msg
withLabels config labels chart =
    let
        background =
            Svg.rect [ width "1450", height "1000", fill config.background ] []
    in
        Svg.svg
            [ viewBox "0 0 1450 1000", Svg.Attributes.style "width: 100%; height:auto;" ]
            [ background, chart, drawLabels config labels ]


drawLabels : Config -> List LabelEntry -> Svg msg
drawLabels config slices =
    let
        labels slices2 =
            List.indexedMap (labelRow config) slices2
    in
        Svg.g [] (labels slices)


labelRow : Config -> Int -> LabelEntry -> Svg msg
labelRow config index ( color, label ) =
    let
        xBase =
            1000 + 50

        paddingTop =
            100 + (index * 70)

        colorDimensions =
            30

        displayText =
            if String.length label > 30 then
                (String.left 27 label) ++ "..."
            else
                label
    in
        Svg.g
            []
            [ Svg.rect
                [ x <| String.fromInt <| xBase
                , y <| String.fromInt <| (paddingTop - (floor <| colorDimensions / 2))
                , width <| String.fromInt <| colorDimensions
                , height <| String.fromInt <| colorDimensions
                , fill color
                ]
                []
            , text_
                [ x <| String.fromInt <| (xBase + colorDimensions + 20)
                , y <| String.fromInt <| paddingTop
                , fill config.labelsColor
                , fontFamily "sans-serif"
                , fontSize "25px"
                , alignmentBaseline "middle"
                ]
                [ text displayText ]
            ]
