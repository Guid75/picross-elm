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
import Dict exposing (Dict)
import Matrix exposing (Matrix)
import Http
import Time exposing (second)
import Array
import Animation
import Animation.Messenger
import Ease
import Grid exposing (Grid)
import Types exposing (GridCoord, FloatCoord, Coord, CellType(..), Cell, CellSelection, Level)
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
    | BoardSizeResult ( Float, Float, Float, Float )
    | GetLevels (Result Http.Error (List Level))
    | Cheat
    | ChoseLevel String
    | Animate Animation.Msg
    | SvgMouseMove Coord
    | SvgMouseLeave
    | TransMousePosResult ( Float, Float )
    | EndOfFade


type State
    = Init
    | Playing
    | Won AnimState


type AnimState
    = WonAnimFadeOut (Animation.Messenger.State Msg)
    | WonAnimShrinking ShrinkAnims


type alias ShrinkAnims =
    Dict ( Int, Int ) ShrinkAnim


type alias ShrinkAnim =
    { anim : Animation.Messenger.State Msg
    , finished : Bool
    }


type alias Model =
    { board : Matrix Cell
    , state : State
    , grid : Grid
    , hoveredCell : Maybe GridCoord
    , selection : Maybe CellSelection
    , levels : Maybe (List Level)
    , currentLevel : Maybe String
    , boundingBox : { x : Float, y : Float, width : Float, height : Float }
    }


gridColor : String
gridColor =
    "black"


init : ( Model, Cmd Msg )
init =
    { board =
        Matrix.repeat 17 20 (Cell Empty False)
    , state = Init
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
    , currentLevel =
        Nothing
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


drawRect : Model -> GridCoord -> Svg msg
drawRect model { col, row } =
    let
        cellCoord =
            Grid.getCellCoord col row model.grid
    in
        rect
            [ x <| toString <| cellCoord.x + 1.0
            , y <| toString <| cellCoord.y + 1.0
            , width <| toString <| model.grid.cellSize - 2.0
            , height <| toString <| model.grid.cellSize - 2.0
            , fill "red"
            ]
            []


drawHovered : Model -> List (Svg msg)
drawHovered model =
    case model.hoveredCell of
        Nothing ->
            []

        Just { col, row } ->
            let
                cellCoord =
                    Grid.getCellCoord col row model.grid
            in
                [ rect
                    [ x <| toString <| cellCoord.x
                    , y <| toString <| cellCoord.y
                    , width <| toString <| model.grid.cellSize
                    , height <| toString <| model.grid.cellSize
                    , fill "blue"
                    ]
                    []
                ]


drawSelected : Model -> GridCoord -> Svg msg
drawSelected model { col, row } =
    let
        cellCoord =
            Grid.getCellCoord col row model.grid

        cellSize =
            model.grid.cellSize

        padding =
            1.0

        color =
            "#383838"

        attrs =
            case model.state of
                Won (WonAnimShrinking shrinkAnims) ->
                    case Dict.get ( col, row ) shrinkAnims of
                        Just shrinkAnim ->
                            Animation.render shrinkAnim.anim

                        Nothing ->
                            []

                _ ->
                    []

        pos =
            case attrs of
                [] ->
                    [ x <| toString <| cellCoord.x + padding
                    , y <| toString <| cellCoord.y + padding
                    ]

                _ ->
                    attrs
    in
        rect
            (List.concat
                [ pos
                , [ width <| toString <| cellSize - 2.0 * padding
                  , height <| toString <| cellSize - 2.0 * padding
                  , fill color
                  ]
                ]
            )
            []


drawRejected : Model -> GridCoord -> Svg msg
drawRejected model { col, row } =
    let
        cellCoord =
            Grid.getCellCoord col row model.grid

        cellSize =
            model.grid.cellSize

        padding =
            4.0

        color =
            "red"

        width =
            "4.0"

        attrs =
            getFadeOutAnimAttrs model
    in
        g
            attrs
            [ line
                [ x1 <| toString <| cellCoord.x + padding
                , y1 <| toString <| cellCoord.y + padding
                , x2 <| toString <| cellCoord.x + cellSize - padding
                , y2 <| toString <| cellCoord.y + cellSize - padding
                , stroke color
                , strokeWidth width
                , strokeOpacity <| toString opacity
                , strokeLinecap "round"
                ]
                []
            , line
                [ x1 <| toString <| cellCoord.x + padding
                , y1 <| toString <| cellCoord.y + cellSize - padding
                , x2 <| toString <| cellCoord.x + cellSize - padding
                , y2 <| toString <| cellCoord.y + padding
                , stroke color
                , strokeWidth width
                , strokeOpacity <| toString opacity
                , strokeLinecap "round"
                ]
                []
            ]


drawCell : Model -> GridCoord -> Cell -> Maybe (Svg msg)
drawCell model gridCoord cell =
    case cell.userChoice of
        Selected ->
            Just <| drawSelected model gridCoord

        Rejected ->
            Just <| drawRejected model gridCoord

        _ ->
            Nothing


drawCells : Model -> List (Svg msg)
drawCells model =
    let
        filterFunc =
            case model.state of
                Won (WonAnimShrinking _) ->
                    \( _, cell ) -> cell.value

                _ ->
                    \( _, cell ) -> cell.userChoice /= Empty
    in
        model.board
            |> Matrix.toIndexedArray
            |> Array.filter filterFunc
            |> Array.map (\( ( col, row ), cell ) -> drawCell model { col = col, row = row } cell)
            |> Array.toList
            |> List.filterMap identity


drawHorizontalLabelsHover : Model -> List (Svg msg)
drawHorizontalLabelsHover model =
    case ( model.hoveredCell, model.state ) of
        ( Just { row }, Playing ) ->
            [ rect
                [ x <| toString <| model.boundingBox.x
                , y <| toString <| .y <| Grid.getCellCoord 0 row model.grid
                , height <| toString <| model.grid.cellSize
                , width <| toString <| -model.boundingBox.x
                , fill "black"
                ]
                []
            ]

        _ ->
            []


drawVerticalLabelsHover : Model -> List (Svg msg)
drawVerticalLabelsHover model =
    case ( model.hoveredCell, model.state ) of
        ( Just { col }, Playing ) ->
            [ rect
                [ x <| toString <| .x <| Grid.getCellCoord col 0 model.grid
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


drawHorizontalLabels : Model -> List (Svg msg)
drawHorizontalLabels model =
    let
        animAttrs =
            getFadeOutAnimAttrs model

        textRight =
            toString <| model.grid.topLeft.x - 2.0

        allTips =
            MatrixUtils.getHorizontalTips <| Matrix.map .value model.board

        getTipsLine : Int -> List Int -> Svg msg
        getTipsLine index tips =
            let
                text =
                    List.map toString tips |> String.join " "

                cellCoord =
                    Grid.getCellCoord 0 index model.grid
            in
                Svg.text_
                    [ y <| toString <| cellCoord.y + model.grid.cellSize / 2.0
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


drawVerticalLabels : Model -> List (Svg msg)
drawVerticalLabels model =
    let
        animAttrs =
            getFadeOutAnimAttrs model

        textBottom =
            toString <| model.grid.topLeft.y - 2.0

        allTips =
            MatrixUtils.getVerticalTips <| Matrix.map .value model.board

        getTipsCol : Int -> List Int -> List (Svg msg)
        getTipsCol index tips =
            let
                cellCoord =
                    Grid.getCellCoord index 0 model.grid
            in
                List.indexedMap
                    (\rowIndex tip ->
                        (Svg.text_
                            [ x <| toString <| cellCoord.x + model.grid.cellSize / 2.0
                            , y <| toString <| cellCoord.y - model.grid.cellSize * (toFloat rowIndex) - model.grid.boldThickness - 2.0
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


selectionToRectangle : CellSelection -> ( GridCoord, GridCoord )
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


selectionToList : CellSelection -> List GridCoord
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


drawSelection : Model -> List (Svg msg)
drawSelection model =
    case model.selection of
        Nothing ->
            []

        Just selection ->
            selection
                |> selectionToList
                |> List.map (drawRect model)


drawWinningLabel : Model -> List (Svg msg)
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


{-| Draw the transparent rectangle that will be used to intercept all mouse
   events relative to the grid
-}
drawGridMouseLayer : Model -> List (Svg Msg)
drawGridMouseLayer model =
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


getFadeOutAnimAttrs : Model -> List (Attribute msg)
getFadeOutAnimAttrs model =
    case model.state of
        Won (WonAnimFadeOut fadeOutAnim) ->
            Animation.render fadeOutAnim

        _ ->
            []


drawGridAndLabels : Model -> List (Svg msg)
drawGridAndLabels model =
    case model.state of
        Won (WonAnimShrinking _) ->
            []

        _ ->
            List.concat
                [ [ Grid.drawGrid model.grid <| getFadeOutAnimAttrs model ]
                , drawHorizontalLabels model
                , drawVerticalLabels model
                ]


viewSvg : Model -> Html Msg
viewSvg model =
    let
        animAttrs =
            getFadeOutAnimAttrs model

        bbox =
            model.boundingBox
    in
        case model.state of
            Init ->
                svg
                    [ id "board"
                    , width "800"
                    , height "600"
                    ]
                    []

            _ ->
                svg
                    [ id "board"
                    , width "800"
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
                      --, shapeRendering "crispEdges"
                    ]
                <|
                    List.concat
                        [ drawGridAndLabels model
                        , drawCells model
                        , drawSelection model
                        , case ( isWinning model, model.selection ) of
                            ( False, Nothing ) ->
                                drawHovered model

                            _ ->
                                []
                        , drawWinningLabel model
                        , drawGridMouseLayer model
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
            [ Html.Events.on "change" levelsDecoder
            , Html.Attributes.value <| Maybe.withDefault "1" model.currentLevel
            ]
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


toggleCell : Model -> GridCoord -> Model
toggleCell model { col, row } =
    case Matrix.get col row model.board of
        Just cell ->
            { model | board = Matrix.set col row { cell | userChoice = Empty } model.board }

        Nothing ->
            model


mouseDownOnGrid : Model -> Model
mouseDownOnGrid model =
    let
        coordToSelection : GridCoord -> CellSelection
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
            Grid.getClosestCell { x = x, y = y } model.grid

        selection =
            Maybe.map (\selection -> { firstCell = selection.firstCell, lastCell = hoveredCell }) model.selection
    in
        { model
            | hoveredCell = Just hoveredCell
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


getFadeOutInitialStyle : Animation.Messenger.State Msg
getFadeOutInitialStyle =
    Animation.style
        [ Animation.opacity 1.0 ]


checkWinning : Model -> Model
checkWinning model =
    case model.state of
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
                            Won <| WonAnimFadeOut getFadeOutInitialStyle
                        else
                            model.state
                }

        _ ->
            model


isWinning : Model -> Bool
isWinning model =
    case model.state of
        Won _ ->
            True

        _ ->
            False


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
                | state =
                    Animation.interrupt
                        [ Animation.toWith
                            (Animation.easing
                                { duration = 0.6 * second
                                , ease = (\x -> x)
                                }
                            )
                            [ Animation.opacity 0
                            ]
                        , Animation.Messenger.send EndOfFade
                        ]
                        getFadeOutInitialStyle
                        |> WonAnimFadeOut
                        |> Won
              }
            , cmd
            )
        else
            ( newModel, cmd )


getCellInitAnim : Model -> GridCoord -> Animation.Messenger.State Msg
getCellInitAnim model { col, row } =
    let
        cellPos =
            Grid.getCellCoord col row model.grid
    in
        Animation.style
            [ Animation.x <| cellPos.x + 1.0
            , Animation.y <| cellPos.y + 1.0
            ]


getFinalCellPosition : Model -> GridCoord -> FloatCoord
getFinalCellPosition model gridCoord =
    let
        finalWidth =
            20.0 * (toFloat model.grid.colCount)

        currentGridWidth =
            Grid.getGridWidth model.grid

        finalHeight =
            20.0 * (toFloat model.grid.rowCount)

        currentGridHeight =
            Grid.getGridHeight model.grid

        topLeftCell =
            { x = (currentGridWidth - finalWidth) / 2.0
            , y = (currentGridHeight - finalHeight) / 2.0
            }
    in
        { x =
            topLeftCell.x + 20.0 * toFloat (gridCoord.col)
        , y =
            topLeftCell.y + 20.0 * toFloat (gridCoord.row)
        }


shrinkAnims : Model -> Model
shrinkAnims model =
    let
        cellsGridCoords =
            model.board
                |> Matrix.toIndexedArray
                |> Array.filter (\( _, cell ) -> cell.value)
                |> Array.map (\( ( col, row ), _ ) -> { col = col, row = row })
                |> Array.toList

        coordToAnim gridCoord =
            let
                finalCellPosition =
                    getFinalCellPosition model gridCoord
            in
                { anim =
                    Animation.interrupt
                        [ Animation.toWith
                            (Animation.easing
                                { duration = 0.7 * second
                                , ease = Ease.outBounce
                                }
                            )
                            [ Animation.x finalCellPosition.x
                            , Animation.y finalCellPosition.y
                            ]
                        ]
                        (getCellInitAnim model gridCoord)
                , finished = False
                }

        anims =
            cellsGridCoords
                |> List.map (\gridCoord -> ( ( gridCoord.col, gridCoord.row ), coordToAnim gridCoord ))
                |> Dict.fromList
    in
        { model | state = Won (WonAnimShrinking anims) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMouseDown button ->
            mouseDownOnGrid model ! []

        BoardMouseUp button ->
            mouseUpOnGrid button model ! []

        GetLevels (Ok levels) ->
            let
                newModel =
                    { model | levels = Just levels }
            in
                case newModel.levels |> Maybe.andThen List.head of
                    Just level ->
                        choseLevel "21" newModel ! [ computeBoardSize () ]

                    Nothing ->
                        newModel ! []

        GetLevels (Err _) ->
            model ! []

        ChoseLevel level ->
            choseLevel level model ! [ computeBoardSize () ]

        Cheat ->
            cheat model ! []

        Animate animMsg ->
            let
                computeNewStyle anim =
                    Animation.Messenger.update animMsg anim
            in
                case model.state of
                    Won (WonAnimFadeOut fadeOutAnim) ->
                        let
                            ( newStyle, cmds ) =
                                computeNewStyle fadeOutAnim
                        in
                            ( { model | state = Won <| WonAnimFadeOut newStyle }, cmds )

                    Won (WonAnimShrinking shrinkAnims) ->
                        let
                            newAnimsAndCmds =
                                Dict.map
                                    (\_ shrinkAnim ->
                                        let
                                            animAndCmd =
                                                computeNewStyle shrinkAnim.anim
                                        in
                                            ( { shrinkAnim | anim = Tuple.first animAndCmd }
                                            , Tuple.second animAndCmd
                                            )
                                    )
                                    shrinkAnims

                            newAnims =
                                Dict.map (\_ animAndCmd -> Tuple.first animAndCmd) newAnimsAndCmds

                            cmds =
                                newAnimsAndCmds
                                    |> Dict.toList
                                    |> List.map (Tuple.second >> Tuple.second)
                        in
                            { model
                                | state =
                                    newAnims |> WonAnimShrinking |> Won
                            }
                                ! cmds

                    _ ->
                        model ! []

        BoardSizeResult ( x, y, width, height ) ->
            { model | boundingBox = { x = x, y = y, width = width, height = height } } ! []

        SvgMouseMove coord ->
            model ! [ requestTransMousePos ( coord.x, coord.y ) ]

        SvgMouseLeave ->
            { model | hoveredCell = Nothing }
                ! []

        TransMousePosResult pos ->
            boardMousePos pos model ! []

        EndOfFade ->
            shrinkAnims model ! []

        NoOp ->
            model ! []


port computeBoardSize : () -> Cmd msg


port computeBoardSizeResult : (( Float, Float, Float, Float ) -> msg) -> Sub msg


port boardMouseDown : (Int -> msg) -> Sub msg


port boardMouseUp : (Int -> msg) -> Sub msg


port requestTransMousePos : ( Int, Int ) -> Cmd msg


port transMousePosResult : (( Float, Float ) -> msg) -> Sub msg


shrinkAnimsToAnimsList : ShrinkAnims -> List (Animation.Messenger.State Msg)
shrinkAnimsToAnimsList shrinkAnims =
    shrinkAnims
        |> Dict.toList
        |> List.map (Tuple.second >> .anim)


{-| Returns an animations list depending on the current model state
-}
computeAnimations : Model -> List (Animation.Messenger.State Msg)
computeAnimations model =
    case model.state of
        Won (WonAnimFadeOut fadeOutAnim) ->
            [ fadeOutAnim ]

        Won (WonAnimShrinking shrinkAnims) ->
            shrinkAnims
                |> Dict.toList
                |> List.map (Tuple.second >> .anim)

        _ ->
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ computeBoardSizeResult BoardSizeResult
        , boardMouseDown BoardMouseDown
        , boardMouseUp BoardMouseUp
        , Animation.subscription Animate <| computeAnimations model
        , transMousePosResult TransMousePosResult
        ]
