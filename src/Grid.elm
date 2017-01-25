module Grid
    exposing
        ( Grid
        , drawGrid
        , getCellCoord
        , getGridHeight
        , getGridWidth
        , getGridTopLeft
        , getClosestCell
        , isInGrid
        )

import Html
import Svg exposing (Svg, line, g)
import Svg.Attributes exposing (..)
import Types exposing (FloatCoord, GridCoord)


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


isInGrid : FloatCoord -> Grid -> Bool
isInGrid coord grid =
    (coord.x >= grid.topLeft.x)
        && (coord.x < grid.topLeft.x + getGridWidth grid)
        && (coord.y >= grid.topLeft.y)
        && (coord.y < grid.topLeft.y + getGridHeight grid)


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


getGridTopLeft : Grid -> FloatCoord
getGridTopLeft grid =
    let
        offset =
            getNthLineOffset grid 0
    in
        { x = offset
        , y = offset
        }


drawGrid : Grid -> List (Html.Attribute msg) -> Svg msg
drawGrid grid animAttrs =
    g
        animAttrs
        (List.concat
            [ (List.foldl (drawVerticalLine grid) [] (List.range 0 grid.colCount))
            , (List.foldl (drawHorizontalLine grid) [] (List.range 0 grid.rowCount))
            ]
        )


getCellCoord : Int -> Int -> Grid -> FloatCoord
getCellCoord col row grid =
    let
        colThickness =
            getThicknessByIndex grid col

        rowThickness =
            getThicknessByIndex grid row
    in
        { x = (getNthLineOffset grid col) + colThickness / 2.0 + grid.topLeft.x
        , y = (getNthLineOffset grid row) + rowThickness / 2.0 + grid.topLeft.y
        }


getClosestRow : Float -> Grid -> Int
getClosestRow y grid =
    let
        getRow : Int -> Int
        getRow row =
            let
                upperHalfThickness =
                    (getThicknessByIndex grid (row + 1)) / 2.0

                upperCoord =
                    getCellCoord 0 (row + 1) grid
            in
                if row >= grid.rowCount then
                    grid.rowCount - 1
                else if y < upperCoord.y - upperHalfThickness then
                    row
                else
                    getRow <| row + 1
    in
        getRow 0


getClosestCol : Float -> Grid -> Int
getClosestCol x grid =
    let
        getCol : Int -> Int
        getCol col =
            let
                rightHalfThickness =
                    (getThicknessByIndex grid (col + 1)) / 2.0

                rightCoord =
                    getCellCoord (col + 1) 0 grid
            in
                if col >= grid.colCount then
                    grid.colCount - 1
                else if x < rightCoord.x - rightHalfThickness then
                    col
                else
                    getCol <| col + 1
    in
        getCol 0


getClosestCell : FloatCoord -> Grid -> GridCoord
getClosestCell { x, y } grid =
    { col = getClosestCol x grid
    , row = getClosestRow y grid
    }
