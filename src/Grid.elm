module Grid
    exposing
        ( Grid
        , drawGrid
        , getCellCoord
        , getCellByXY
        , FloatCoord
        , getGridHeight
        , getGridWidth
        )

import Svg exposing (Svg, line)
import Svg.Attributes exposing (..)


type alias Grid =
    { colCount : Int
    , rowCount : Int
    , boldInterval : Int
    , thinThickness : Float
    , boldThickness : Float
    , cellSize : Float
    , topLeft : FloatCoord
    , strokeColor : String
    }


type alias FloatCoord =
    { x : Float
    , y : Float
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
            [ x1 <| toString <| x + grid.topLeft.x
            , y1 <| toString grid.topLeft.y
            , x2 <| toString <| x + grid.topLeft.x
            , y2 <| toString <| getGridHeight grid + grid.topLeft.y
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
            [ x1 <| toString grid.topLeft.x
            , y1 <| toString <| y + grid.topLeft.y
            , x2 <| toString <| getGridWidth grid + grid.topLeft.x
            , y2 <| toString <| y + grid.topLeft.y
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
        { cellX = (getNthLineOffset grid col) + colThickness / 2.0 + grid.topLeft.x
        , cellY = (getNthLineOffset grid row) + rowThickness / 2.0 + grid.topLeft.y
        }


getColByX : Float -> Grid -> Maybe Int
getColByX x grid =
    let
        getCol : Int -> Maybe Int
        getCol col =
            let
                coord =
                    getCellCoord col 0 grid
            in
                if col >= grid.colCount then
                    Nothing
                else if x >= coord.cellX && x <= coord.cellX + grid.cellSize then
                    Just col
                else
                    getCol <| col + 1
    in
        getCol 0


getRowByY : Float -> Grid -> Maybe Int
getRowByY y grid =
    let
        getRow : Int -> Maybe Int
        getRow row =
            let
                coord =
                    getCellCoord 0 row grid
            in
                if row >= grid.rowCount then
                    Nothing
                else if y >= coord.cellY && y <= coord.cellY + grid.cellSize then
                    Just row
                else
                    getRow <| row + 1
    in
        getRow 0


getCellByXY : Float -> Float -> Grid -> Maybe { col : Int, row : Int }
getCellByXY x y grid =
    let
        maybeCol =
            getColByX x grid

        maybeRow =
            getRowByY y grid
    in
        case ( maybeCol, maybeRow ) of
            ( Just col, Just row ) ->
                Just { col = col, row = row }

            _ ->
                Nothing
