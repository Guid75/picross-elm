port module Picross exposing (..)

import Html exposing (text, div, Html, input)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Json.Decode as Json
import Matrix exposing (Matrix)
import Array
import Mouse
import Grid exposing (Grid)


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
    | MouseMove { x : Int, y : Int }
    | BoardMouseDown Int
    | BoardMouseUp Int
    | BoardMousePos ( Float, Float )
    | BoldThicknessChanged String
    | ThinThicknessChanged String
    | CellSizeChanged String


type alias CellSelection =
    { firstCell : Coord
    , lastCell : Coord
    }


type alias Model =
    { board : Matrix Cell
    , grid : Grid
    , hoveredCell : Maybe Coord
    , selection : Maybe CellSelection
    }


type alias Coord =
    { col : Int
    , row : Int
    }


type CellType
    = Selected
    | Rejected
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
        Selected ->
            "#383838"

        Rejected ->
            "red"

        -- "black"
        Empty ->
            "darkgray"


init : ( Model, Cmd Msg )
init =
    { board =
        Matrix.repeat 17 20 (Cell Empty)
    , grid =
        { colCount = 17
        , rowCount = 20
        , boldInterval = 5
        , boldThickness = 3.0
        , thinThickness = 1.0
        , strokeColor = gridColor
        , cellSize = 20.0
        }
    , hoveredCell = Nothing
    , selection = Nothing
    }
        ! []


drawRect : Model -> Coord -> Svg Msg
drawRect model { col, row } =
    let
        { cellX, cellY } =
            Grid.getCellCoord col row model.grid
    in
        rect
            [ x <| toString <| cellX + 1.0
            , y <| toString <| cellY + 1.0
            , width <| toString <| model.grid.cellSize - 2.0
            , height <| toString <| model.grid.cellSize - 2.0
            , fill "red"
            ]
            []


drawHovered : Model -> List (Svg Msg)
drawHovered model =
    case model.hoveredCell of
        Nothing ->
            []

        Just { col, row } ->
            let
                { cellX, cellY } =
                    Grid.getCellCoord col row model.grid
            in
                [ rect
                    [ x <| toString <| cellX
                    , y <| toString <| cellY
                    , width <| toString <| model.grid.cellSize
                    , height <| toString <| model.grid.cellSize
                    , fill "blue"
                    ]
                    []
                ]


drawSelected : { cellX : Float, cellY : Float } -> Float -> Float -> String -> List (Svg Msg)
drawSelected { cellX, cellY } cellSize padding fillColor =
    [ rect
        [ x <| toString <| cellX + padding
        , y <| toString <| cellY + padding
        , width <| toString <| cellSize - 2.0 * padding
        , height <| toString <| cellSize - 2.0 * padding
        , fill fillColor
        ]
        []
    ]


drawRejected : { cellX : Float, cellY : Float } -> Float -> List (Svg Msg)
drawRejected { cellX, cellY } cellSize =
    let
        padding =
            4.0

        color =
            "red"

        width =
            "4.0"
    in
        [ line
            [ x1 <| toString <| cellX + padding
            , y1 <| toString <| cellY + padding
            , x2 <| toString <| cellX + cellSize - padding
            , y2 <| toString <| cellY + cellSize - padding
            , stroke color
            , strokeWidth width
            , strokeLinecap "round"
            ]
            []
        , line
            [ x1 <| toString <| cellX + padding
            , y1 <| toString <| cellY + cellSize - padding
            , x2 <| toString <| cellX + cellSize - padding
            , y2 <| toString <| cellY + padding
            , stroke color
            , strokeWidth width
            , strokeLinecap "round"
            ]
            []
        ]


drawCell : Model -> Coord -> Cell -> Svg Msg
drawCell model { col, row } cell =
    let
        cellPos =
            Grid.getCellCoord col row model.grid

        fillColor =
            case model.hoveredCell of
                Just coord ->
                    if coord.col == col && coord.row == row then
                        "blue"
                    else
                        colorByCellType cell.cellType

                Nothing ->
                    colorByCellType cell.cellType

        padding =
            case cell.cellType of
                Empty ->
                    0.0

                _ ->
                    1.0
    in
        g
            []
            (case cell.cellType of
                Selected ->
                    drawSelected cellPos model.grid.cellSize padding fillColor

                Rejected ->
                    drawRejected cellPos model.grid.cellSize

                _ ->
                    []
            )


drawCells : Model -> List (Svg Msg)
drawCells model =
    model.board
        |> Matrix.toIndexedArray
        |> Array.filter (\( _, cell ) -> cell.cellType /= Empty)
        |> Array.map (\( ( col, row ), cell ) -> drawCell model { col = col, row = row } cell)
        |> Array.toList


selectionToRectangle : CellSelection -> ( Coord, Coord )
selectionToRectangle selection =
    let
        ( col1, col2 ) =
            if selection.firstCell.col <= selection.lastCell.col then
                ( selection.firstCell.col, selection.lastCell.col )
            else
                ( selection.lastCell.col, selection.firstCell.col )

        ( row1, row2 ) =
            if selection.firstCell.row <= selection.lastCell.row then
                ( selection.firstCell.row, selection.lastCell.row )
            else
                ( selection.lastCell.row, selection.firstCell.row )
    in
        ( { col = col1, row = row1 }, { col = col2, row = row2 } )


selectionToList : CellSelection -> List Coord
selectionToList selection =
    let
        ( topLeft, bottomRight ) =
            selectionToRectangle selection

        colList =
            List.map ((+) topLeft.col) <| List.range 0 (bottomRight.col - topLeft.col)

        rowList =
            List.map ((+) topLeft.row) <| List.range 0 (bottomRight.row - topLeft.row)

        foldRows col l =
            List.foldl (\row l -> { col = col, row = row } :: l) l rowList
    in
        List.foldl foldRows [] colList


drawSelection : Model -> List (Svg Msg)
drawSelection model =
    case model.selection of
        Nothing ->
            []

        Just selection ->
            selection
                |> selectionToList
                |> List.map (drawRect model)


viewSvg : Model -> Html Msg
viewSvg model =
    svg
        [ id "board"
        , width "1024"
        , height "500"
        , viewBox "0 0 1024 500"
          --        , shapeRendering "crispEdges"
          -- , onMouseDown MouseDownOnGrid
          -- , onMouseUp MouseUpOnGrid
        ]
    <|
        List.concat
            [ [ g [] <| Grid.drawGrid model.grid ]
            , drawCells model
            , drawHovered model
            , drawSelection model
            ]


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
                , Html.Attributes.value <| toString model.grid.boldThickness
                , Html.Events.onInput BoldThicknessChanged
                ]
                []
            , Html.text <| toString model.grid.boldThickness
            ]
        , div
            []
            [ Html.text "Thin thickness"
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "10"
                , Html.Attributes.value <| toString model.grid.thinThickness
                , Html.Events.onInput ThinThicknessChanged
                ]
                []
            , Html.text <| toString model.grid.thinThickness
            ]
        , div
            []
            [ Html.text "Cell size"
            , Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "40"
                , Html.Attributes.value <| toString model.grid.cellSize
                , Html.Events.onInput CellSizeChanged
                ]
                []
            , Html.text <| toString model.grid.cellSize
            ]
        ]


toggleCell : Model -> Coord -> Model
toggleCell model { col, row } =
    case Matrix.get col row model.board of
        Just cell ->
            { model | board = Matrix.set col row { cell | cellType = Empty } model.board }

        Nothing ->
            model


mouseDownOnGrid : Model -> Model
mouseDownOnGrid model =
    let
        coordToSelection : Coord -> CellSelection
        coordToSelection coord =
            { firstCell = coord, lastCell = coord }
    in
        { model | selection = Maybe.map coordToSelection model.hoveredCell }


updateBoardWithSelection : Model -> CellType -> Matrix Cell
updateBoardWithSelection model cellType =
    let
        toggleValue value =
            if value == cellType then
                Empty
            else
                cellType

        setCell value { col, row } board =
            Matrix.update
                col
                row
                (\_ ->
                    Cell value
                )
                board
    in
        case model.selection of
            Nothing ->
                model.board

            Just selection ->
                let
                    selList =
                        selectionToList selection

                    setValue =
                        Matrix.get selection.firstCell.col selection.firstCell.row model.board
                            |> Maybe.map (.cellType >> toggleValue)
                            |> Maybe.withDefault Selected
                in
                    List.foldr (setCell setValue) model.board selList


mouseUpOnGrid : Int -> Model -> Model
mouseUpOnGrid button model =
    let
        value =
            case button of
                1 ->
                    Selected

                _ ->
                    Rejected

        board =
            updateBoardWithSelection model value
    in
        { model
            | selection = Nothing
            , board = board
        }


boardMousePos : ( Float, Float ) -> Model -> Model
boardMousePos ( x, y ) model =
    let
        hoveredCell =
            Grid.getCellByXY x y model.grid

        selection =
            Maybe.map (\selection -> { firstCell = selection.firstCell, lastCell = Maybe.withDefault selection.lastCell hoveredCell }) model.selection
    in
        { model
            | hoveredCell = hoveredCell
            , selection = selection
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMouseDown button ->
            mouseDownOnGrid model ! []

        BoardMouseUp button ->
            mouseUpOnGrid button model ! []

        MouseMove pos ->
            ( model, requestBoardMousePos ( pos.x, pos.y ) )

        BoldThicknessChanged value ->
            let
                grid =
                    model.grid

                newGrid =
                    { grid | boldThickness = Result.withDefault 2.0 (String.toFloat value) }
            in
                { model | grid = newGrid } ! []

        ThinThicknessChanged value ->
            let
                grid =
                    model.grid

                newGrid =
                    { grid | thinThickness = Result.withDefault 2.0 (String.toFloat value) }
            in
                { model | grid = newGrid } ! []

        CellSizeChanged value ->
            let
                grid =
                    model.grid

                newGrid =
                    { grid | cellSize = Result.withDefault 2.0 (String.toFloat value) }
            in
                { model | grid = newGrid } ! []

        BoardMousePos pos ->
            boardMousePos pos model ! []

        NoOp ->
            model ! []


port boardMousePosResult : (( Float, Float ) -> msg) -> Sub msg


port requestBoardMousePos : ( Int, Int ) -> Cmd msg


port boardMouseDown : (Int -> msg) -> Sub msg


port boardMouseUp : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , boardMousePosResult BoardMousePos
        , boardMouseDown BoardMouseDown
        , boardMouseUp BoardMouseUp
        ]
