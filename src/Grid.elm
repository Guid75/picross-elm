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


drawVerticalLine : Grid -> Int -> List (Svg msg) -> List (Svg msg)
drawVerticalLine grid colIndex lines =
    let
        gridHeight =
            getGridHeight grid

        x =
            getNthLineOffset grid colIndex

        thickness =
            if colIndex % grid.boldInterval == 0 then
                grid.boldThickness
            else
                grid.thinThickness
    in
        (line
            [ x1 <| toString x
            , y1 "0.0"
            , x2 <| toString x
            , y2 <| toString <| gridHeight
            , stroke grid.strokeColor
            , strokeWidth <| toString thickness
            ]
            []
        )
            :: lines


drawHorizontalLine : Grid -> Int -> List (Svg msg) -> List (Svg msg)
drawHorizontalLine grid rowIndex lines =
    let
        gridWidth =
            getGridWidth grid

        y =
            getNthLineOffset grid rowIndex

        thickness =
            if rowIndex % grid.boldInterval == 0 then
                grid.boldThickness
            else
                grid.thinThickness
    in
        (line
            [ x1 <| "0.0"
            , y1 <| toString y
            , x2 <| toString <| gridWidth
            , y2 <| toString y
            , stroke grid.strokeColor
            , strokeWidth <| toString thickness
            ]
            []
        )
            :: lines


getNthLineOffset : Grid -> Int -> Float
getNthLineOffset { cellSize, thinThickness, boldThickness, boldInterval } lineNumber =
    let
        dec =
            (lineNumber + boldInterval - 1) // boldInterval

        thickness =
            if lineNumber % boldInterval == 0 then
                boldThickness
            else
                thinThickness
    in
        toFloat lineNumber * cellSize + toFloat (lineNumber - dec) * thinThickness + toFloat dec * boldThickness + thickness / 2.0


getGridWidth : Grid -> Float
getGridWidth grid =
    let
        lastLineIndex =
            grid.colCount

        offset =
            getNthLineOffset grid lastLineIndex

        thickness =
            if lastLineIndex % grid.boldInterval == 0 then
                grid.boldThickness
            else
                grid.thinThickness
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
            if lastLineIndex % grid.boldInterval == 0 then
                grid.boldThickness
            else
                grid.thinThickness
    in
        offset + thickness / 2.0


drawGrid : Grid -> List (Svg msg)
drawGrid grid =
    let
        gridWidth =
            getGridWidth grid

        gridHeight =
            getGridHeight grid
    in
        List.concat
            [ (List.foldl (drawVerticalLine grid) [] (List.range 0 grid.colCount))
            , (List.foldl (drawHorizontalLine grid) [] (List.range 0 grid.rowCount))
            ]


getCellCoord : Int -> Int -> Grid -> { cellX : Float, cellY : Float }
getCellCoord col row grid =
    let
        colThickness =
            if col % grid.boldInterval == 0 then
                grid.boldThickness
            else
                grid.thinThickness

        rowThickness =
            if row % grid.boldInterval == 0 then
                grid.boldThickness
            else
                grid.thinThickness
    in
        { cellX = (getNthLineOffset grid col) + colThickness / 2.0
        , cellY = (getNthLineOffset grid row) + rowThickness / 2.0
        }
