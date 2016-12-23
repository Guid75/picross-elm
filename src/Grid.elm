module Grid exposing (Grid, drawGrid, getCellCoord)

import Svg exposing (Svg, line)
import Svg.Attributes exposing (..)


type alias Grid =
    { colCount : Int
    , rowCount : Int
    , boldInterval : Int
    , thinThickness : Float
    , boldThickness : Float
    , cellSize : Float
    , strokeColor : String
    }


getThicknessByIndex : Grid -> Int -> Float
getThicknessByIndex grid index =
    if index % grid.boldInterval == 0 then
        grid.boldThickness
    else
        grid.thinThickness


drawVerticalLine : Grid -> Int -> List (Svg msg) -> List (Svg msg)
drawVerticalLine grid colIndex lines =
    let
        x =
            getNthLineOffset grid colIndex
    in
        (line
            [ x1 <| toString x
            , y1 "0.0"
            , x2 <| toString x
            , y2 <| toString <| getGridHeight grid
            , stroke grid.strokeColor
            , strokeWidth <| toString <| getThicknessByIndex grid colIndex
            ]
            []
        )
            :: lines


drawHorizontalLine : Grid -> Int -> List (Svg msg) -> List (Svg msg)
drawHorizontalLine grid rowIndex lines =
    let
        y =
            getNthLineOffset grid rowIndex
    in
        (line
            [ x1 <| "0.0"
            , y1 <| toString y
            , x2 <| toString <| getGridWidth grid
            , y2 <| toString y
            , stroke grid.strokeColor
            , strokeWidth <| toString <| getThicknessByIndex grid rowIndex
            ]
            []
        )
            :: lines


getNthLineOffset : Grid -> Int -> Float
getNthLineOffset grid lineNumber =
    let
        dec =
            (lineNumber + grid.boldInterval - 1) // grid.boldInterval

        thickness =
            getThicknessByIndex grid lineNumber
    in
        toFloat lineNumber * grid.cellSize + toFloat (lineNumber - dec) * grid.thinThickness + toFloat dec * grid.boldThickness + thickness / 2.0


getGridWidth : Grid -> Float
getGridWidth grid =
    let
        lastLineIndex =
            grid.colCount

        offset =
            getNthLineOffset grid lastLineIndex

        thickness =
            getThicknessByIndex grid lastLineIndex
    in
        offset + thickness / 2.0


getGridHeight : Grid -> Float
getGridHeight grid =
    let
        lastLineIndex =
            grid.rowCount

        offset =
            getNthLineOffset grid lastLineIndex

        thickness =
            getThicknessByIndex grid lastLineIndex
    in
        offset + thickness / 2.0


drawGrid : Grid -> List (Svg msg)
drawGrid grid =
    List.concat
        [ (List.foldl (drawVerticalLine grid) [] (List.range 0 grid.colCount))
        , (List.foldl (drawHorizontalLine grid) [] (List.range 0 grid.rowCount))
        ]


getCellCoord : Int -> Int -> Grid -> { cellX : Float, cellY : Float }
getCellCoord col row grid =
    let
        colThickness =
            getThicknessByIndex grid col

        rowThickness =
            getThicknessByIndex grid row
    in
        { cellX = (getNthLineOffset grid col) + colThickness / 2.0
        , cellY = (getNthLineOffset grid row) + rowThickness / 2.0
        }
