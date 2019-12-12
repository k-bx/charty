module Charty.LineChart exposing
    ( Dataset
    , Series
    , DataPoint
    , Config
    , defaults
    , view
    )

{-| This module is in charge of drawing line charts.


# Data representation

@docs Dataset
@docs Series
@docs DataPoint


# Settings

@docs Config
@docs defaults


# Drawing

@docs view

-}

import Array exposing (Array)
import Charty.ArrayUtil as ArrayUtil
import Charty.Color as Color exposing (Color)
import Charty.Labels as Labels
import Charty.SelectList as SL exposing (include, maybe)
import List.Extra
import Regex
import Round
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| A dataset is just a list of series.
-}
type alias Dataset =
    List Series


{-| A series of points that will be draw as a separate line
-}
type alias Series =
    { label : String
    , data : List DataPoint
    }


{-| An (x,y) pair that will be drawn in the line chart
-}
type alias DataPoint =
    ( Float, Float )


type alias SvgPoint =
    ( Float, Float )


type alias Transform =
    DataPoint -> SvgPoint


{-| Configuration for how the chart will be drawn. Note that
[`LineChart.defaults`](Charty-LineChart#defaults) can be used as a base
configuration.
-}
type alias Config =
    { drawPoints : Bool
    , background : Color
    , colorAssignment : Dataset -> List ( Color, Series )
    , labelPrecision : Int
    , drawLabels : Bool
    }


type alias DrawingSettings =
    -- These are settings for how to draw the data that are inferred from the actual dataset
    { padding : Padding
    , transform : Transform
    , yLabels : Array Float
    }


type alias Padding =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


type alias DatasetBounds =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


{-| Default configuration. Labels have at most two decimal places and a default
color palette is used.
-}
defaults : Config
defaults =
    { drawPoints = True
    , background = "transparent"
    , colorAssignment = Color.assignDefaults
    , labelPrecision = 2
    , drawLabels = True
    }


{-| This function generates svg markup for the chart, provided a the necessary
configuration and dataset. Example usage:

    sampleDataset : LineChart.Dataset
    sampleDataset =
        [ [ ( 100000, 3 ), ( 100001, 4 ) ]
        , [ ( 100000, 1 ), ( 100001, 2.5 ) ]
        ]

    view : Model -> Html Msg
    view model =
        Html.div
            []
            [ Html.p [] [ Html.text "Wow!" ]
            , LineChart.view LineChart.defaults dataset
            ]

-}
view : Config -> Dataset -> Svg msg
view cfg dataset =
    let
        background =
            Svg.rect [ width "1000", height "1000", fill cfg.background ] []

        drawingSettings =
            initDrawingSettings cfg dataset

        seriesWithColors =
            cfg.colorAssignment dataset

        lines =
            List.map (drawLine drawingSettings.transform) seriesWithColors

        points =
            if cfg.drawPoints then
                List.map (drawPoints drawingSettings.transform) seriesWithColors

            else
                []

        chart attrs =
            Svg.svg
                (viewBox "0 0 1000 1000" :: attrs)
                [ background
                , axis cfg drawingSettings
                , g [] lines
                , g [] points
                ]
    in
    if cfg.drawLabels then
        seriesWithColors
            |> List.map (\( color, series ) -> ( color, series.label ))
            |> addLabels cfg (chart [ width "1000" ])

    else
        chart [ width "100%", height "100%" ]


addLabels : Config -> Svg msg -> List Labels.LabelEntry -> Svg msg
addLabels cfg chart labels =
    Labels.withLabels
        { background = cfg.background, labelsColor = "#333333" }
        labels
        chart


initDrawingSettings : Config -> Dataset -> DrawingSettings
initDrawingSettings cfg dataset =
    let
        points =
            List.concatMap .data dataset

        xs =
            List.map (\( x, y ) -> x) points

        ys =
            List.map (\( x, y ) -> y) points

        restCase =
            let
                padding =
                    { top = 50, right = 50, bottom = 50, left = 50 }
            in
            { padding = padding
            , transform = initTransform { xMin = 0, xMax = 0, yMin = 0, yMax = 0 } padding
            , yLabels = Array.fromList [ 0 ]
            }
    in
    case ( List.minimum xs, List.maximum xs ) of
        ( Just xMin, Just xMax ) ->
            case ( List.minimum ys, List.maximum ys ) of
                ( Just yMin, Just yMax ) ->
                    let
                        yLabels =
                            initYLabels yMin yMax
                    in
                    case ( List.head (Array.toList yLabels), List.Extra.last (Array.toList yLabels) ) of
                        ( Just yFirst, Just yLast ) ->
                            let
                                bounds =
                                    { xMin = xMin
                                    , xMax = xMax
                                    , yMin = yFirst
                                    , yMax = yLast
                                    }

                                padding =
                                    initPadding cfg yLabels
                            in
                            case padding of
                                Just jpadding ->
                                    { padding = jpadding
                                    , transform = initTransform bounds jpadding
                                    , yLabels = yLabels
                                    }

                                Nothing ->
                                    restCase

                        _ ->
                            restCase

                _ ->
                    restCase

        _ ->
            restCase


initYLabels : Float -> Float -> Array Float
initYLabels yMin yMax =
    if yMin == yMax then
        [ 0, yMin, 2 * yMin ]
            |> List.sort
            |> Array.fromList

    else
        let
            splitRange min max pieces =
                if pieces == 0 then
                    [ min ]

                else
                    let
                        -- note that it's better to recalculate the step
                        -- every time to make up for rounding errors.
                        -- this way we make sure that ranges are more evenly
                        -- distributed and -more important- that the last
                        -- element is precisely the upperbound we wanted
                        step =
                            (max - min) / toFloat pieces
                    in
                    min :: splitRange (min + step) max (pieces - 1)
        in
        Array.fromList (splitRange yMin yMax 6)


initPadding : Config -> Array Float -> Maybe Padding
initPadding cfg yLabels =
    let
        labelOffset =
            label cfg.labelPrecision
                >> gsub "\\." ""
                >> String.length
                >> (\n -> toFloat n * 20)

        leftOffset =
            yLabels
                |> Array.map labelOffset
                |> Array.toList
                |> List.maximum
    in
    case leftOffset of
        Nothing ->
            Nothing

        Just lo ->
            Just
                { top = 50
                , right = 50
                , bottom = 50
                , left = lo
                }


gsub : String -> String -> String -> String
gsub regex replacement s =
    case Regex.fromString regex of
        Nothing ->
            s

        Just regexObj ->
            Regex.replace regexObj (always replacement) s


label : Int -> Float -> String
label precision =
    Round.ceiling precision >> gsub "\\.0+$" ""


initTransform : DatasetBounds -> Padding -> Transform
initTransform { xMin, xMax, yMin, yMax } { top, right, bottom, left } =
    let
        drawingWidth =
            1000 - right - left

        drawingHeight =
            1000 - top - bottom

        scaleFactor vm vM v =
            if vm == vM then
                0.5

            else
                (v - vm) / (vM - vm)
    in
    \( x, y ) ->
        ( left + drawingWidth * scaleFactor xMin xMax x
        , (1000 - top) - drawingHeight * scaleFactor yMin yMax y
        )


axis : Config -> DrawingSettings -> Svg msg
axis cfg drawingSettings =
    let
        { top, right, bottom, left } =
            drawingSettings.padding

        axisLine ( vx1, vy1 ) ( vx2, vy2 ) =
            line
                [ x1 <| vx1
                , y1 <| vy1
                , x2 <| vx2
                , y2 <| vy2
                , stroke "#CFCFCF"
                , strokeDasharray "5 5"
                ]
                []

        referenceLine yVal =
            let
                yT =
                    Tuple.second <| drawingSettings.transform ( 0, yVal )
            in
            g []
                [ axisLine ( String.fromFloat left, String.fromFloat yT ) ( String.fromFloat (1000 - right), String.fromFloat yT )
                , text_
                    [ x <| String.fromFloat (left - 15)
                    , y <| String.fromFloat (yT + 8)
                    , textAnchor "end"
                    , fontFamily "Oxygen,Helvetica,Arial,sans-serif"
                    , fontSize "24px"
                    , fill "#CFCFCF"
                    ]
                    [ text (label cfg.labelPrecision yVal) ]
                ]

        yLabels =
            Array.foldr (\l r -> referenceLine l :: r) [] drawingSettings.yLabels

        yAxis =
            axisLine ( String.fromFloat left, String.fromFloat bottom ) ( String.fromFloat left, String.fromFloat (1000 - top) )
    in
    g [] (yAxis :: yLabels)


drawLine : Transform -> ( Color, Series ) -> Svg msg
drawLine transform ( color, series ) =
    let
        pointString ( x, y ) =
            x ++ " " ++ y

        attr =
            series.data
                |> List.map (transform >> (\( a, b ) -> ( String.fromFloat a, String.fromFloat b )) >> pointString)
                |> String.join ", "
    in
    polyline [ points attr, stroke color, fill "transparent" ] []


drawPoints : Transform -> ( Color, Series ) -> Svg msg
drawPoints transform ( color, series ) =
    g [] <|
        List.map (drawPoint transform color) series.data


drawPoint : Transform -> Color -> DataPoint -> Svg msg
drawPoint transform color point =
    let
        ( x, y ) =
            transform point

        handle event setting =
            Maybe.map (\f -> event (f point)) setting
    in
    circle
        (SL.select
            [ include <| cx (String.fromFloat x)
            , include <| cy (String.fromFloat y)
            , include <| r "10"
            , include <| fill color
            ]
        )
        []
