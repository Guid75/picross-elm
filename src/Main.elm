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
    | ClickOnGrid
    | ClickOnCell Coord
    | MouseMove { x : Int, y : Int }
    | BoardMousePos ( Int, Int )
    | BoldThicknessChanged String
    | ThinThicknessChanged String
    | CellSizeChanged String


type alias Model =
    { board : Matrix Cell
    , grid : Grid
    , clickCount : Int
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
            "#383838" -- "black"

        Empty ->
            "darkgray"


init : ( Model, Cmd Msg )
init =
    { board =
        Matrix.repeat 17 20 (Cell Full)
    , clickCount = 0
    , grid =
        { colCount = 17
        , rowCount = 20
        , boldInterval = 5
        , boldThickness = 3.0
        , thinThickness = 1.0
        , strokeColor = gridColor
        , cellSize = 20.0
        }
    }
        ! []


drawCell : Model -> Coord -> Cell -> Svg Msg
drawCell model { col, row } cell =
    let
        { cellX, cellY } =
            Grid.getCellCoord col row model.grid

        colThickness =
            if col % 5 == 0 then
                model.grid.boldThickness
            else
                model.grid.thinThickness

        rowThickness =
            if row % 5 == 0 then
                model.grid.boldThickness
            else
                model.grid.thinThickness
    in
        rect
            [ x <| toString <| cellX + colThickness / 2.0 + 1.0
            , y <| toString <| cellY + rowThickness / 2.0 + 1.0
            , width <| toString <| model.grid.cellSize - 2.0
            , height <| toString <| model.grid.cellSize - 2.0
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
            [ g [] <| Grid.drawGrid model.grid ]
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
