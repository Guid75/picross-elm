port module Picross exposing (..)

import Html exposing (text, div, Html, input)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseMove)
import Matrix exposing (Matrix)
import Array
import Mouse


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | ClickOnGrid
    | ClickOnCell Coord
    | MouseMove { x : Int, y : Int }
    | BoardMousePos ( Int, Int )
    | BoldThicknessChanged String
    | ThinThicknessChanged String
    | CellSizeChanged String


type alias Model =
    { board : Matrix Cell
    , cellSize : Float
    , clickCount : Int
    , boldThickness : Float
    , thinThickness : Float
    }


type alias Coord =
    { col : Int
    , row : Int
    }


type CellType
    = Full
    | Empty


type alias Cell =
    { cellType : CellType }


gridThinStroke : Float
gridThinStroke =
    1.0


gridBoldStroke : Float
gridBoldStroke =
    3.0


gridColor : String
gridColor =
    "black"


colorByCellType : CellType -> String
colorByCellType cellType =
    case cellType of
        Full ->
            "black"

        Empty ->
            "darkgray"


init : ( Model, Cmd Msg )
init =
    { board =
        Matrix.repeat 17 20 (Cell Full)
    , cellSize = 20.0
    , clickCount = 0
    , boldThickness = 2.0
    , thinThickness = 1.0
    }
        ! []


drawCell : Model -> Coord -> Cell -> Svg Msg
drawCell model { col, row } cell =
    let
        cellX =
            getNthLineOffset model.cellSize model.thinThickness model.boldThickness col

        cellY =
            getNthLineOffset model.cellSize model.thinThickness model.boldThickness row

        colThickness =
            if col % 5 == 0 then
                model.boldThickness
            else
                model.thinThickness

        rowThickness =
            if row % 5 == 0 then
                model.boldThickness
            else
                model.thinThickness
    in
        rect
            [ x <| toString <| cellX + colThickness / 2.0 + 1.0
            , y <| toString <| cellY + rowThickness / 2.0 + 1.0
            , width <| toString <| model.cellSize - 2.0
            , height <| toString <| model.cellSize - 2.0
            , fill <| colorByCellType cell.cellType
              -- , strokeWidth "0.5"
              -- , strokeDasharray dashArray
              -- , stroke "red"
            , onClick (ClickOnCell { col = col, row = row })
            ]
            []


drawCells : Model -> List (Svg Msg)
drawCells model =
    model.board
        |> Matrix.toIndexedArray
        |> Array.filter (\( _, cell ) -> cell.cellType /= Empty)
        |> Array.map (\( ( col, row ), cell ) -> drawCell model { col = col, row = row } cell)
        |> Array.toList


drawVerticalLine : Model -> Float -> Int -> List (Svg Msg) -> List (Svg Msg)
drawVerticalLine model gridHeight colIndex lines =
    let
        x =
            getNthLineOffset model.cellSize model.thinThickness model.boldThickness colIndex

        thickness =
            if colIndex % 5 == 0 then
                model.boldThickness
            else
                model.thinThickness
    in
        (line
            [ x1 <| toString x
            , y1 "0.0"
            , x2 <| toString x
            , y2 <| toString <| gridHeight
            , stroke gridColor
            , strokeWidth <| toString thickness
            ]
            []
        )
            :: lines


drawHorizontalLine : Model -> Float -> Int -> List (Svg Msg) -> List (Svg Msg)
drawHorizontalLine model gridWidth rowIndex lines =
    let
        y =
            getNthLineOffset model.cellSize model.thinThickness model.boldThickness rowIndex

        thickness =
            if rowIndex % 5 == 0 then
                model.boldThickness
            else
                model.thinThickness
    in
        (line
            [ x1 <| "0.0"
            , y1 <| toString y
            , x2 <| toString <| gridWidth
            , y2 <| toString y
            , stroke gridColor
            , strokeWidth <| toString thickness
            ]
            []
        )
            :: lines


getNthLineOffset : Float -> Float -> Float -> Int -> Float
getNthLineOffset cellSize thinThickness boldThickness lineNumber =
    let
        dec =
            if lineNumber == 0 then
                0
            else
                (lineNumber - 1) // 5 + 1

        thickness =
            if lineNumber % 5 == 0 then
                boldThickness
            else
                thinThickness
    in
        toFloat lineNumber * cellSize + toFloat (lineNumber - dec) * thinThickness + toFloat dec * boldThickness + thickness / 2.0


getGridWidth : Model -> Float
getGridWidth model =
    let
        lastLineIndex =
            Matrix.width model.board

        offset =
            getNthLineOffset model.cellSize model.thinThickness model.boldThickness lastLineIndex

        thickness =
            if lastLineIndex % 5 == 0 then
                model.boldThickness
            else
                model.thinThickness
    in
        offset + thickness / 2.0


getGridHeight : Model -> Float
getGridHeight model =
    let
        lastLineIndex =
            Matrix.height model.board

        offset =
            getNthLineOffset model.cellSize model.thinThickness model.boldThickness lastLineIndex

        thickness =
            if lastLineIndex % 5 == 0 then
                model.boldThickness
            else
                model.thinThickness
    in
        offset + thickness / 2.0


drawEmptyGrid : Model -> Svg Msg
drawEmptyGrid model =
    let
        gridWidth =
            getGridWidth model

        gridHeight =
            getGridHeight model
    in
        g
            []
            (List.concat
                [ (List.foldl (drawVerticalLine model gridHeight) [] (List.range 0 (Matrix.width model.board)))
                , (List.foldl (drawHorizontalLine model gridWidth) [] (List.range 0 (Matrix.height model.board)))
                ]
            )


viewSvg : Model -> Html Msg
viewSvg model =
    svg
        [ id "board"
        , width "1024"
        , height "500"
        , viewBox "0 0 1024 500"
          --        , shapeRendering "crispEdges"
        , onClick ClickOnGrid
        ]
    <|
        List.append
            [ drawEmptyGrid model ]
            (drawCells model)


view : Model -> Html Msg
view model =
    div
        []
        [ viewSvg model
        , div
            []
            [ Html.text "Bold thickness"
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "10"
                , Html.Attributes.value <| toString model.boldThickness
                , Html.Events.onInput BoldThicknessChanged
                ]
                []
            , Html.text <| toString model.boldThickness
            ]
        , div
            []
            [ Html.text "Thin thickness"
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "10"
                , Html.Attributes.value <| toString model.thinThickness
                , Html.Events.onInput ThinThicknessChanged
                ]
                []
            , Html.text <| toString model.thinThickness
            ]
        , div
            []
            [ Html.text "Cell size"
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "40"
                , Html.Attributes.value <| toString model.cellSize
                , Html.Events.onInput CellSizeChanged
                ]
                []
            , Html.text <| toString model.cellSize
            ]
        ]



-- (drawCells model)
-- List.append (drawCells model)
--     []
-- text_ [ x "150", y "134", stroke "black", fill "black", fontSize "20px" ] [ Svg.text "1 12 3 4" ] ]


toggleCell : Model -> Coord -> Model
toggleCell model { col, row } =
    case Matrix.get col row model.board of
        Just cell ->
            { model | board = Matrix.set col row { cell | cellType = Empty } model.board }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        ClickOnGrid ->
            { model | clickCount = model.clickCount + 1 } ! []

        ClickOnCell coord ->
            toggleCell model coord ! []

        MouseMove pos ->
            ( model, requestBoardMousePos ( pos.x, pos.y ) )

        BoldThicknessChanged value ->
            { model | boldThickness = Result.withDefault 2.0 (String.toFloat value) } ! []

        ThinThicknessChanged value ->
            { model | thinThickness = Result.withDefault 2.0 (String.toFloat value) } ! []

        CellSizeChanged value ->
            { model | cellSize = Result.withDefault 2.0 (String.toFloat value) } ! []

        BoardMousePos pos ->
            model ! []

        NoOp ->
            model ! []


port boardMousePosResult : (( Int, Int ) -> msg) -> Sub msg


port requestBoardMousePos : ( Int, Int ) -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , boardMousePosResult BoardMousePos
        ]
