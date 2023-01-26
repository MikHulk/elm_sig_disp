module Graph exposing (..)

import Html.Styled
    exposing
        ( Html
        , fromUnstyled
        )
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import SinOsc


type alias Info =
    { x : Float
    , y : Float
    }


chart : Int -> (Int -> Info) -> Html msg
chart samples funk =
    fromUnstyled <|
        LineChart.viewCustom
            { y = Axis.default 250 "" .y
            , x = Axis.default 1200 "time" .x
            , container =
                Container.styled
                    "line-chart-1"
                    [ ( "font-family", "monospace" ) ]
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.default
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.default
            , dots = Dots.default
            }
            [ LineChart.line Colors.red Dots.none "signal" <|
                List.map funk <|
                    List.range 0 samples
            ]
