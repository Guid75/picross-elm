port module Picross exposing (..)

import Html exposing (select, option, text, div, Html, input)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Json.Decode as Json
import Matrix exposing (Matrix)
import Http
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
    | LevelsList (Result Http.Error (List String))
    | ChoseLevel String


type alias CellSelection =
    { firstCell : Coord
    , lastCell : Coord
    }


type alias Model =
    { board : Matrix Cell
    , grid : Grid
    , hoveredCell : Maybe Coord
    , selection : Maybe CellSelection
    , levelsList : Maybe (List String)
    , currentLevel : Maybe String
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
    { userChoice : CellType
    , value : Bool
    }


gridThinStroke : Float
gridThinStroke =
    1.0


gridBoldStroke : Float
gridBoldStroke =
    3.0


gridColor : String
gridColor =
    "black"


init : ( Model, Cmd Msg )
init =
    { board =
        Matrix.repeat 17 20 (Cell Empty False)
    , grid =
        { colCount = 17
        , rowCount = 20
        , boldInterval = 5
        , boldThickness = 3.0
        , thinThickness = 1.0
        , strokeColor = gridColor
        , cellSize = 20.0
        , topLeft = { x = 200.0, y = 50.0 }
        }
    , hoveredCell = Nothing
    , selection = Nothing
    , levelsList = Nothing
    , currentLevel = Nothing
    }
        ! [ getLevelsList ]


decodeLevelsList : Json.Decoder (List String)
decodeLevelsList =
    Json.field "levels" (Json.list Json.string)


getLevelsList : Cmd Msg
getLevelsList =
    let
        url =
            "levels/levels.json"

        request =
            Http.get url decodeLevelsList
    in
        Http.send LevelsList request


-- decodeLevel : Json.Decoder (Matrix Cell)
-- decodeLevel =
    
              
-- loadLevel : String -> Cmd Msg
-- loadLevel name =
--     let
--         url =
--             "levels/" ++ name ++ ".json"

--         request =
--             Http.get url decodeLevel
--     in
--         Http.send LevelLoaded request


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


drawSelected : { cellX : Float, cellY : Float } -> Float -> Float -> List (Svg Msg)
drawSelected { cellX, cellY } cellSize opacity =
    let
        padding =
            1.0

        color =
            "#383838"
    in
        [ rect
            [ x <| toString <| cellX + padding
            , y <| toString <| cellY + padding
            , width <| toString <| cellSize - 2.0 * padding
            , height <| toString <| cellSize - 2.0 * padding
            , fill color
            , fillOpacity <| toString opacity
            ]
            []
        ]


drawRejected : { cellX : Float, cellY : Float } -> Float -> Float -> List (Svg Msg)
drawRejected { cellX, cellY } cellSize opacity =
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
            , strokeOpacity <| toString opacity
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
            , strokeOpacity <| toString opacity
            , strokeLinecap "round"
            ]
            []
        ]


drawCell : Model -> Coord -> Cell -> Float -> Svg Msg
drawCell model { col, row } cell opacity =
    let
        cellPos =
            Grid.getCellCoord col row model.grid
    in
        g
            []
            (case cell.userChoice of
                Selected ->
                    drawSelected cellPos model.grid.cellSize opacity

                Rejected ->
                    drawRejected cellPos model.grid.cellSize opacity

                _ ->
                    []
            )


drawCells : Model -> List (Svg Msg)
drawCells model =
    model.board
        |> Matrix.toIndexedArray
        |> Array.filter (\( _, cell ) -> cell.userChoice /= Empty)
        |> Array.map (\( ( col, row ), cell ) -> drawCell model { col = col, row = row } cell 1.0)
        |> Array.toList


drawLabels : Model -> List (Svg Msg)
drawLabels model =
    let
        cellPos =
            Grid.getCellCoord 0 1 model.grid

        textRight =
            toString <| model.grid.topLeft.x - 2.0
    in
        [ g
            [ textAnchor "end"
            , fontSize <| (toString <| model.grid.cellSize) ++ "px"
            ]
            [ Svg.text_
                [ x textRight
                , y <| toString <| (Grid.getCellCoord 0 0 model.grid).cellY + model.grid.cellSize / 2.0
                ]
                [ tspan
                    [ dominantBaseline "central" ]
                    -- baselineShift "-0.5ex"
                    [ Svg.text "11 4 5 " ]
                ]
              -- , line
              --     [ x1 "0.0"
              --     , y1 <| toString <| (Grid.getCellCoord 0 0 model.grid).cellY + model.grid.cellSize / 2.0
              --     , x2 textRight
              --     , y2 <| toString <| (Grid.getCellCoord 0 0 model.grid).cellY + model.grid.cellSize / 2.0
              --     , stroke "red"
              --     ]
              --     []
            , Svg.text_
                [ x textRight
                , y <| toString <| cellPos.cellY + model.grid.cellSize - 4.0
                ]
                [ Svg.text "1 1 2 4 5 " ]
            , Svg.text_
                [ x textRight
                , y <| toString <| (Grid.getCellCoord 0 2 model.grid).cellY + model.grid.cellSize - 4.0
                ]
                [ Svg.text "1 1 1 14 5 " ]
            ]
        ]


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
        ]
    <|
        List.concat
            [ [ g [] <| Grid.drawGrid model.grid ]
            , drawLabels model
            , drawCells model
            , drawSelection model
            , drawHovered model
            ]


levelsDecoder : Json.Decoder Msg
levelsDecoder =
    Json.map ChoseLevel Html.Events.targetValue


levelsCombo : Model -> Html Msg
levelsCombo model =
    let
        options =
            case model.levelsList of
                Nothing ->
                    [ option [] [ Html.text "<no levels>" ] ]

                Just levelsList ->
                    List.map (\name -> option [ Html.Attributes.value name ] [ Html.text name ]) levelsList
    in
        select
            [ Html.Events.on "change" levelsDecoder ]
            options


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ Html.text "Left button to select cells, "
            , Html.text "Right button to reject cells"
            ]
        , viewSvg model
        , div
            []
            [ Html.text "Levels ", levelsCombo model ]
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
            { model | board = Matrix.set col row { cell | userChoice = Empty } model.board }

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
                (\cell ->
                    { cell | userChoice = value }
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
                            |> Maybe.map (.userChoice >> toggleValue)
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

        LevelsList (Ok levelsList) ->
            { model | levelsList = Just levelsList } ! []

        LevelsList (Err _) ->
            model ! []

        ChoseLevel level ->
            { model | currentLevel = Just level } ! [] -- loadLevel level ]

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
