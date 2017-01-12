port module Picross exposing (..)

import Html exposing (select, option, text, div, Html, input, button)
import Html.Attributes exposing (disabled)
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Json.Decode exposing (int, string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import List.Extra
import Matrix exposing (Matrix)
import Http
import Time exposing (second)
import Array
import Animation
import Grid exposing (Grid)
import Types exposing (Coord, FloatCoord, CellType(..), Cell, CellSelection, Level)
import MatrixUtils


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = winningUpdateWrapper update
        , subscriptions = subscriptions
        }


type Msg
    = NoOp
    | BoardMouseDown Int
    | BoardMouseUp Int
    | BoardMousePos ( Float, Float )
    | BoardSizeResult ( Float, Float, Float, Float )
    | GetLevels (Result Http.Error (List Level))
    | Cheat
    | ChoseLevel String
    | Animate Animation.Msg
    | SvgMouseMove Coord
    | SvgMouseLeave
    | TransMousePosResult ( Float, Float )


type State
    = Playing
    | Won


type alias Model =
    { board : Matrix Cell
    , state : State
    , grid : Grid
    , hoveredCell : Maybe Coord
    , selection : Maybe CellSelection
    , levels : Maybe (List Level)
    , currentLevel : Maybe String
    , fadeOutAnim : Animation.State
    , boundingBox : { x : Float, y : Float, width : Float, height : Float }
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
    , state = Playing
    , grid =
        { colCount = 17
        , rowCount = 20
        , boldInterval = 5
        , boldThickness = 3.0
        , thinThickness = 1.0
        , strokeColor = gridColor
        , cellSize = 20.0
        , topLeft = { x = 0.0, y = 0.0 }
        }
    , hoveredCell = Nothing
    , selection = Nothing
    , levels = Nothing
    , currentLevel = Nothing
    , fadeOutAnim =
        Animation.style
            [ Animation.opacity 1.0 ]
    , boundingBox = { x = 0.0, y = 0.0, width = 0.0, height = 0.0 }
    }
        ! [ getLevels ]


int2BoolConverter : Decoder Bool
int2BoolConverter =
    Json.Decode.map (\v -> v > 0) int


decodeLevel : Decoder Level
decodeLevel =
    decode Level
        |> required "name" Json.Decode.string
        |> optional "description" Json.Decode.string ""
        |> required "content" (list <| list int2BoolConverter)


decodeLevels : Decoder (List Level)
decodeLevels =
    list decodeLevel


getLevels : Cmd Msg
getLevels =
    let
        url =
            "levels/levels.json"

        request =
            Http.get url decodeLevels
    in
        Http.send GetLevels request


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


getCellPos : Int -> Int -> Model -> { cellX : Float, cellY : Float }
getCellPos col row model =
    Grid.getCellCoord col row model.grid


drawCell : Model -> Coord -> Cell -> Float -> Svg Msg
drawCell model { col, row } cell opacity =
    let
        cellPos =
            getCellPos col row model

        attrs =
            if cell.userChoice == Rejected then
                Animation.render model.fadeOutAnim
            else
                []
    in
        g
            attrs
        <|
            List.concat
                [ (case cell.userChoice of
                    Selected ->
                        drawSelected cellPos model.grid.cellSize opacity

                    Rejected ->
                        drawRejected cellPos model.grid.cellSize opacity

                    _ ->
                        []
                  )
                , []
                ]


drawCells : Model -> List (Svg Msg)
drawCells model =
    model.board
        |> Matrix.toIndexedArray
        |> Array.filter (\( _, cell ) -> cell.userChoice /= Empty)
        |> Array.map (\( ( col, row ), cell ) -> drawCell model { col = col, row = row } cell 1.0)
        |> Array.toList


drawHorizontalLabelsHover : Model -> List (Svg Msg)
drawHorizontalLabelsHover model =
    case ( model.hoveredCell, model.state ) of
        ( Just { row }, Playing ) ->
            [ rect
                [ x <| toString <| model.boundingBox.x
                , y <| toString <| (Grid.getCellCoord 0 row model.grid).cellY
                , height <| toString <| model.grid.cellSize
                , width <| toString <| -model.boundingBox.x
                , fill "black"
                ]
                []
            ]

        _ ->
            []


drawVerticalLabelsHover : Model -> List (Svg Msg)
drawVerticalLabelsHover model =
    case ( model.hoveredCell, model.state ) of
        ( Just { col }, Playing ) ->
            [ rect
                [ x <| toString <| (Grid.getCellCoord col 0 model.grid).cellX
                , y <| toString <| model.boundingBox.y
                , height <| toString <| -model.boundingBox.y
                , width <| toString <| model.grid.cellSize
                , fill "black"
                ]
                []
            ]

        _ ->
            []


isHoveredRow : Model -> Int -> Bool
isHoveredRow model testRow =
    case model.hoveredCell of
        Just { row } ->
            testRow == row

        Nothing ->
            False


isHoveredCol : Model -> Int -> Bool
isHoveredCol model testCol =
    case model.hoveredCell of
        Just { col } ->
            testCol == col

        Nothing ->
            False


drawHorizontalLabels : Model -> List (Svg Msg)
drawHorizontalLabels model =
    let
        animAttrs =
            Animation.render model.fadeOutAnim

        textRight =
            toString <| model.grid.topLeft.x - 2.0

        allTips =
            MatrixUtils.getHorizontalTips <| Matrix.map .value model.board

        getTipsLine : Int -> List Int -> Svg Msg
        getTipsLine index tips =
            let
                text =
                    List.map toString tips |> String.join " "
            in
                Svg.text_
                    [ y <| toString <| (Grid.getCellCoord 0 index model.grid).cellY + model.grid.cellSize / 2.0
                    , fill <|
                        if isHoveredRow model index then
                            "white"
                        else
                            "black"
                    ]
                    [ tspan
                        [ dominantBaseline "central" ]
                        [ Svg.text text ]
                    ]

        bbox =
            model.boundingBox
    in
        List.append
            (drawHorizontalLabelsHover model)
            [ g
                (animAttrs
                    ++ [ textAnchor "end"
                       , fontSize <| (toString <| model.grid.cellSize) ++ "px"
                       , x textRight
                       , fill "white"
                       ]
                )
                (List.indexedMap getTipsLine allTips)
            ]


drawVerticalLabels : Model -> List (Svg Msg)
drawVerticalLabels model =
    let
        animAttrs =
            Animation.render model.fadeOutAnim

        textBottom =
            toString <| model.grid.topLeft.y - 2.0

        allTips =
            MatrixUtils.getVerticalTips <| Matrix.map .value model.board

        getTipsCol : Int -> List Int -> List (Svg Msg)
        getTipsCol index tips =
            let
                coord =
                    Grid.getCellCoord index 0 model.grid
            in
                List.indexedMap
                    (\rowIndex tip ->
                        (Svg.text_
                            [ x <| toString <| coord.cellX + model.grid.cellSize / 2.0
                            , y <| toString <| coord.cellY - model.grid.cellSize * (toFloat rowIndex) - model.grid.boldThickness - 2.0
                            , fill <|
                                if isHoveredCol model index then
                                    "white"
                                else
                                    "black"
                            ]
                            [ Svg.text <| toString tip ]
                        )
                    )
                    (List.reverse tips)
    in
        List.append
            (drawVerticalLabelsHover model)
            [ g
                (animAttrs
                    ++ [ textAnchor "middle"
                       , fontSize <| (toString <| model.grid.cellSize) ++ "px"
                       ]
                )
                (List.concat (List.indexedMap getTipsCol allTips))
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


drawWinningLabel : Model -> List (Svg Msg)
drawWinningLabel model =
    let
        gridWidth =
            Grid.getGridWidth model.grid

        gridHeight =
            Grid.getGridHeight model.grid
    in
        case isWinning model of
            True ->
                [ text_
                    [ x <| toString <| model.grid.topLeft.x + gridWidth / 2.0
                    , y <| toString <| model.grid.topLeft.y + gridHeight + model.grid.cellSize
                    , textAnchor "middle"
                    , fontSize <| (toString <| model.grid.cellSize) ++ "px"
                    , fill "red"
                    ]
                    [ Svg.text "You win!" ]
                ]

            False ->
                []


mouseEvent : Decoder Coord
mouseEvent =
    decode Coord
        |> required "clientX" Json.Decode.int
        |> required "clientY" Json.Decode.int


drawOverRect : Model -> List (Svg Msg)
drawOverRect model =
    let
        gridTopLeft =
            Grid.getGridTopLeft model.grid

        gridHeight =
            Grid.getGridHeight model.grid

        gridWidth =
            Grid.getGridWidth model.grid
    in
        [ rect
            [ x <| toString <| model.grid.topLeft.x
            , y <| toString <| model.grid.topLeft.y
            , width <| toString <| gridWidth
            , height <| toString <| gridHeight
            , fill "red"
            , strokeWidth "0.0"
            , opacity "0.0"
            , Svg.Events.on "mousemove" (Json.Decode.map SvgMouseMove mouseEvent)
            , Svg.Events.on "mouseleave" (Json.Decode.succeed SvgMouseLeave)
            ]
            []
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    let
        animAttrs =
            Animation.render model.fadeOutAnim

        bbox =
            model.boundingBox
    in
        svg
            [ id "board"
            , width "1200"
            , height "600"
            , viewBox <|
                (toString bbox.x)
                    ++ " "
                    ++ (toString bbox.y)
                    ++ " "
                    ++ (toString bbox.width)
                    ++ " "
                    ++ (toString bbox.height)
            , preserveAspectRatio "xMinYMin meet"
              --        , shapeRendering "crispEdges"
            ]
        <|
            List.concat
                [ [ Grid.drawGrid model.grid animAttrs ]
                , drawHorizontalLabels model
                , drawVerticalLabels model
                , drawCells model
                , drawSelection model
                , case isWinning model of
                    True ->
                        []

                    False ->
                        drawHovered model
                , drawWinningLabel model
                , drawOverRect model
                ]


levelsDecoder : Decoder Msg
levelsDecoder =
    Json.Decode.map ChoseLevel Html.Events.targetValue


levelsCombo : Model -> Html Msg
levelsCombo model =
    let
        getLevelCaption level =
            if level.description == "" then
                level.name
            else
                level.name ++ " (" ++ level.description ++ ")"

        options =
            case model.levels of
                Nothing ->
                    [ option [] [ Html.text "<no levels>" ] ]

                Just levels ->
                    List.map
                        (\level ->
                            option
                                [ Html.Attributes.value level.name ]
                                [ Html.text <| getLevelCaption level ]
                        )
                        levels
    in
        select
            [ Html.Events.on "change" levelsDecoder ]
            options


view : Model -> Html Msg
view model =
    div
        []
        [ viewSvg model
        , div
            []
            [ Html.text "Levels "
            , levelsCombo model
            , button
                [ onClick Cheat
                , disabled <| isWinning model
                ]
                [ Html.text "resolve it! (cheat)" ]
            , button
                [ onClick (ChoseLevel <| Maybe.withDefault "1" model.currentLevel) ]
                [ Html.text "restart it!" ]
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
        if isWinning model then
            model
        else
            { model | selection = Maybe.map coordToSelection model.hoveredCell }


updateBoardWithSelection : Model -> CellType -> Matrix Cell
updateBoardWithSelection model cellType =
    let
        toggleValue value =
            if value == cellType then
                Empty
            else
                cellType

        overrideValue cellValue value =
            if cellValue == Selected && value == Rejected then
                Selected
            else
                value

        setCell value { col, row } board =
            Matrix.update
                col
                row
                (\cell ->
                    { cell | userChoice = overrideValue cell.userChoice value }
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
    case isWinning model of
        True ->
            model

        False ->
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


applyBoolBoard : Model -> Matrix Bool -> Model
applyBoolBoard model board =
    let
        boolToCell bool =
            { userChoice = Empty, value = bool }

        grid =
            model.grid
    in
        { model
            | board = Matrix.map boolToCell board
            , grid =
                { grid
                    | colCount = Matrix.width board
                    , rowCount = Matrix.height board
                }
        }


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (.name >> (==) name) levels


choseLevel : String -> Model -> Model
choseLevel levelName model =
    let
        maybeModel =
            model.levels
                |> Maybe.andThen (getLevelByName levelName)
                |> Maybe.andThen (\level -> Matrix.fromList level.content)
                |> Maybe.andThen (\board -> Just <| applyBoolBoard model board)
                |> Maybe.andThen (\model -> Just { model | currentLevel = Just levelName, state = Playing })
    in
        Maybe.withDefault model maybeModel


checkWinning : Model -> Model
checkWinning model =
    case model.state of
        Won ->
            model

        Playing ->
            let
                isWrongCell cell =
                    (cell.value && cell.userChoice /= Selected)
                        || (not cell.value && cell.userChoice == Selected)

                won =
                    Array.isEmpty <| Matrix.filter isWrongCell model.board
            in
                { model
                    | state =
                        if won then
                            Won
                        else
                            model.state
                }


isWinning : Model -> Bool
isWinning model =
    model.state == Won


cheat : Model -> Model
cheat model =
    let
        resolve : Cell -> Cell
        resolve cell =
            if cell.value then
                { cell | userChoice = Selected }
            else
                { cell | userChoice = Rejected }
    in
        { model | board = Matrix.map resolve model.board }


winningUpdateWrapper : (Msg -> Model -> ( Model, Cmd Msg )) -> Msg -> Model -> ( Model, Cmd Msg )
winningUpdateWrapper updateFunc msg model =
    let
        oldWinning =
            isWinning model

        ( updateModel, cmd ) =
            updateFunc msg model

        newModel =
            checkWinning updateModel
    in
        if isWinning newModel && not oldWinning then
            ( { newModel
                | fadeOutAnim =
                    Animation.interrupt
                        [ Animation.toWith
                            (Animation.easing
                                { duration = 1 * second
                                , ease = (\x -> x)
                                }
                            )
                            [ Animation.opacity 0
                            ]
                        ]
                        model.fadeOutAnim
              }
            , cmd
            )
        else
            ( newModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMouseDown button ->
            mouseDownOnGrid model ! []

        BoardMouseUp button ->
            mouseUpOnGrid button model ! []

        BoardMousePos pos ->
            boardMousePos pos model ! []

        GetLevels (Ok levels) ->
            let
                newModel =
                    { model | levels = Just levels }
            in
                case newModel.levels |> Maybe.andThen List.head of
                    Just level ->
                        choseLevel level.name newModel ! [ computeBoardSize () ]

                    Nothing ->
                        newModel ! []

        GetLevels (Err _) ->
            model ! []

        ChoseLevel level ->
            let
                newModel =
                    choseLevel level model
            in
                { newModel
                    | fadeOutAnim =
                        Animation.style
                            [ Animation.opacity 1.0
                            ]
                }
                    ! [ computeBoardSize () ]

        Cheat ->
            cheat model ! []

        Animate animMsg ->
            { model
                | fadeOutAnim = Animation.update animMsg model.fadeOutAnim
            }
                ! []

        BoardSizeResult ( x, y, width, height ) ->
            { model | boundingBox = { x = x, y = y, width = width, height = height } } ! []

        SvgMouseMove coord ->
            model ! [ requestTransMousePos ( coord.col, coord.row ) ]

        SvgMouseLeave ->
            { model | hoveredCell = Nothing }
                ! []

        TransMousePosResult pos ->
            boardMousePos pos model ! []

        NoOp ->
            model ! []


port boardMousePosResult : (( Float, Float ) -> msg) -> Sub msg


port requestBoardMousePos : ( Int, Int ) -> Cmd msg


port computeBoardSize : () -> Cmd msg


port computeBoardSizeResult : (( Float, Float, Float, Float ) -> msg) -> Sub msg


port boardMouseDown : (Int -> msg) -> Sub msg


port boardMouseUp : (Int -> msg) -> Sub msg


port requestTransMousePos : ( Int, Int ) -> Cmd msg


port transMousePosResult : (( Float, Float ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ boardMousePosResult BoardMousePos
        , computeBoardSizeResult BoardSizeResult
        , boardMouseDown BoardMouseDown
        , boardMouseUp BoardMouseUp
        , Animation.subscription Animate [ model.fadeOutAnim ]
        , transMousePosResult TransMousePosResult
        ]
