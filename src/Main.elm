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
import Mouse
import Array
import Animation
import Animation.Messenger
import Ease
import Grid exposing (Grid)
import Types exposing (..)
import MatrixUtils
import Msg exposing (Msg(..), LevelChooserMsg(..))
import Model exposing (Model, State(..), AnimState(..), ShrinkAnims)
import LevelChooser


type alias Flags =
    { doneLevels : List String
    , foo : Int
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init =
            (\{ doneLevels, foo } -> Model.init doneLevels ! [ getLevels ])
        , view = view
        , update = winningUpdateWrapper update
        , subscriptions = subscriptions
        }


int2BoolDecoder : Decoder Bool
int2BoolDecoder =
    Json.Decode.map ((==) 1) int


decodeLevel : Decoder Level
decodeLevel =
    decode Level
        |> required "name" Json.Decode.string
        |> required "uuid" Json.Decode.string
        |> optional "description" Json.Decode.string ""
        |> required "content" (list <| list int2BoolDecoder)


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


concreteGridCoord : Maybe GridCoord -> GridCoord
concreteGridCoord maybeGridCoord =
    Maybe.withDefault (GridCoord -1 -1) maybeGridCoord


drawHorizontalLabels : Model -> List (Svg msg)
drawHorizontalLabels model =
    let
        animAttrs =
            getFadeOutAnimAttrs model

        textRight =
            toString <| model.grid.topLeft.x - 2.0

        allTips =
            MatrixUtils.getHorizontalTips <| Matrix.map .value model.board

        hoveredCell =
            concreteGridCoord model.hoveredCell

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
                        if hoveredCell.row == index then
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
                       , pointerEvents "none"
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

        hoveredCell =
            concreteGridCoord model.hoveredCell

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
                                if index == hoveredCell.col then
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
                       , pointerEvents "none"
                       ]
                )
                (List.concat (List.indexedMap getTipsCol allTips))
            ]


sizeRange : Int -> Int -> List Int
sizeRange start size =
    List.range start <| start + size - 1


selectionToList : CellSelection -> List GridCoord
selectionToList selection =
    let
        area =
            selectionToArea selection

        colList =
            sizeRange area.topLeft.col area.size.width

        rowList =
            sizeRange area.topLeft.row area.size.height

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
            Grid.computeGridWidth model.grid

        gridHeight =
            Grid.computeGridHeight model.grid
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


getSvgContent : Model -> ( List (Svg.Attribute Msg), List (Svg Msg) )
getSvgContent model =
    case model.state of
        Init ->
            ( [], [] )

        ChoosingLevel levelChooserModel ->
            ( [ viewBox "0.0 0.0 800.0 600.0" ]
            , LevelChooser.view levelChooserModel
            )

        _ ->
            let
                bbox =
                    model.boundingBox
            in
                ( [ viewBox <|
                        (toString bbox.x)
                            ++ " "
                            ++ (toString bbox.y)
                            ++ " "
                            ++ (toString bbox.width)
                            ++ " "
                            ++ (toString bbox.height)
                  , preserveAspectRatio "xMinYMin meet"
                  ]
                , List.concat
                    [ drawGridAndLabels model
                    , drawCells model
                    , drawSelection model
                    , case ( isWinning model, model.selection ) of
                        ( False, Nothing ) ->
                            drawHovered model

                        _ ->
                            []
                    , drawWinningLabel model
                    ]
                )


mouseButtonDecoder : (MouseButton -> Msg) -> Decoder Msg
mouseButtonDecoder msg =
    Json.Decode.field "which" int
        |> Json.Decode.map buttonIndexToMouseButton
        |> Json.Decode.map msg


contextMenuSuppressor : Html.Attribute Msg
contextMenuSuppressor =
    Html.Events.onWithOptions
        "contextmenu"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.succeed NoOp)


viewSvg : Model -> Html Msg
viewSvg model =
    let
        stopOptions =
            { stopPropagation = True, preventDefault = True }

        baseAttributes =
            [ id "board"
            , width "800"
            , height "600"
            , Html.Events.onWithOptions "mousedown" { stopPropagation = False, preventDefault = True } <| mouseButtonDecoder BoardMouseDown
            , Svg.Events.on "mouseleave" (Json.Decode.succeed SvgMouseLeave)
            , contextMenuSuppressor
            ]

        ( augmentAttributes, contentList ) =
            getSvgContent model
    in
        svg
            (baseAttributes ++ augmentAttributes)
            contentList


levelsDecoder : Decoder Msg
levelsDecoder =
    Json.Decode.map ChoseLevel Html.Events.targetValue


view : Model -> Html Msg
view model =
    div
        []
        [ viewSvg model
        , case model.state of
            ChoosingLevel _ ->
                Html.text ""

            _ ->
                div
                    []
                    [ button
                        [ onClick GoToLevelChooser
                        ]
                        [ Html.text "Return to levels menu" ]
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


buttonIndexToMouseButton : Int -> MouseButton
buttonIndexToMouseButton buttonIndex =
    case buttonIndex of
        1 ->
            LeftButton

        _ ->
            RightButton


mouseDownOnGrid : Model -> Model
mouseDownOnGrid model =
    let
        coordToSelection : GridCoord -> CellSelection
        coordToSelection coord =
            { fixed = coord, floating = coord }
    in
        if isWinning model then
            model
        else
            { model | selection = Maybe.map coordToSelection model.hoveredCell }


mouseDown : MouseButton -> Model -> Model
mouseDown button model =
    let
        newModel =
            { model
                | mouseButtonDown = Just button
                , downSvgMousePos = model.currentSvgMousePos
            }
    in
        case model.state of
            Playing ->
                mouseDownOnGrid newModel

            ChoosingLevel levelChooserModel ->
                { newModel | state = ChoosingLevel <| LevelChooser.mouseDown levelChooserModel }

            _ ->
                newModel


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
                        Matrix.get selection.fixed.col selection.fixed.row model.board
                            |> Maybe.map (.userChoice >> toggleValue)
                            |> Maybe.withDefault Selected
                in
                    List.foldr (setCell setValue) model.board selList


mouseUp : Model -> Model
mouseUp model =
    case model.mouseButtonDown of
        Nothing ->
            model

        Just mouseButton ->
            case isWinning model of
                True ->
                    { model | mouseButtonDown = Nothing }

                False ->
                    let
                        value =
                            case mouseButton of
                                LeftButton ->
                                    Selected

                                RightButton ->
                                    Rejected

                        board =
                            updateBoardWithSelection model value
                    in
                        { model
                            | selection = Nothing
                            , board = board
                            , mouseButtonDown = Nothing
                        }


boardMousePosOnGrid : ( Float, Float ) -> Model -> Model
boardMousePosOnGrid ( x, y ) model =
    let
        closestCell =
            Grid.getClosestCell { x = x, y = y } model.grid

        hoveredCell =
            if Grid.isInGrid { x = x, y = y } model.grid then
                Just closestCell
            else
                Nothing

        selection =
            Maybe.map (\selection -> { fixed = selection.fixed, floating = closestCell }) model.selection
    in
        { model
            | hoveredCell = hoveredCell
            , selection = selection
        }


boardMousePos : ( Float, Float ) -> Model -> Model
boardMousePos ( x, y ) model =
    let
        newModel =
            { model | currentSvgMousePos = Just { x = x, y = y } }
    in
        case ( model.state, model.mouseButtonDown ) of
            ( Playing, _ ) ->
                boardMousePosOnGrid ( x, y ) newModel

            ( ChoosingLevel levelChooserModel, Just LeftButton ) ->
                let
                    currentPos =
                        Maybe.withDefault { x = 0.0, y = 0.0 } newModel.currentSvgMousePos

                    downPos =
                        Maybe.withDefault { x = 0.0, y = 0.0 } newModel.downSvgMousePos
                in
                    { newModel
                        | state = ChoosingLevel <| LevelChooser.mouseMoveWithLeftButton downPos currentPos levelChooserModel
                    }

            _ ->
                newModel


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


getLevelByUuid : String -> List Level -> Maybe Level
getLevelByUuid uuid levels =
    List.Extra.find (.uuid >> (==) uuid) levels


choseLevel : String -> Model -> Model
choseLevel levelUuid model =
    let
        maybeModel =
            model.levels
                |> Maybe.andThen (getLevelByUuid levelUuid)
                |> Maybe.andThen (\level -> Matrix.fromList level.content)
                |> Maybe.andThen (\board -> Just <| applyBoolBoard model board)
                |> Maybe.andThen (\model -> Just { model | currentLevel = Just levelUuid, state = Playing })
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

        newDoneLevels =
            storeLevelToDoneLevels (Maybe.withDefault "1" newModel.currentLevel) model.doneLevels
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
                , doneLevels = newDoneLevels
              }
            , Cmd.batch [ cmd, save <| ( newDoneLevels, 4 ) ]
            )
        else
            ( newModel, cmd )


storeLevelToDoneLevels : String -> List String -> List String
storeLevelToDoneLevels levelUuid levels =
    case List.member levelUuid levels of
        True ->
            levels

        False ->
            levelUuid :: levels


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
            Grid.computeGridWidth model.grid

        finalHeight =
            20.0 * (toFloat model.grid.rowCount)

        currentGridHeight =
            Grid.computeGridHeight model.grid

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


sortBySize : List Level -> List Level
sortBySize levels =
    let
        getLevelSize : Level -> Int
        getLevelSize level =
            let
                rowsCount =
                    List.length level.content

                firstCol =
                    Maybe.withDefault [] <| List.head level.content

                colsCount =
                    List.length firstCol
            in
                rowsCount * colsCount

        compareFunc : Level -> Level -> Order
        compareFunc level1 level2 =
            let
                level1Size =
                    getLevelSize level1

                level2Size =
                    getLevelSize level2
            in
                if level1Size < level2Size then
                    LT
                else if level1Size == level2Size then
                    EQ
                else
                    GT
    in
        List.sortWith compareFunc levels


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardMouseDown button ->
            mouseDown button model ! []

        GetLevels (Ok levels) ->
            { model
                | levels = Just <| sortBySize levels
                , state = ChoosingLevel <| LevelChooser.init (sortBySize levels) 800.0 600.0 model.doneLevels
            }
                ! []

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

        SvgMouseLeave ->
            { model
                | hoveredCell = Nothing
                , currentSvgMousePos = Nothing
            }
                ! []

        SvgMousePosResult pos ->
            boardMousePos pos model ! []

        EndOfFade ->
            shrinkAnims model ! []

        MousePos { x, y } ->
            model ! [ requestSvgMousePos ( x, y ) ]

        MouseUp { x, y } ->
            mouseUp model ! []

        LevelChooserMsg levelChooserMsg ->
            case model.state of
                ChoosingLevel levelChooserModel ->
                    case levelChooserMsg of
                        MouseUpOnTile levelUuid ->
                            if model.currentSvgMousePos == model.downSvgMousePos then
                                choseLevel levelUuid model ! [ computeBoardSize () ]
                            else
                                model ! []

                _ ->
                    model ! []

        GoToLevelChooser ->
            { model | state = ChoosingLevel <| LevelChooser.init (Maybe.withDefault [] model.levels) 800.0 600.0 model.doneLevels } ! []

        NoOp ->
            model ! []


port computeBoardSize : () -> Cmd msg


port computeBoardSizeResult : (( Float, Float, Float, Float ) -> msg) -> Sub msg


port requestSvgMousePos : ( Int, Int ) -> Cmd msg


port svgMousePosResult : (( Float, Float ) -> msg) -> Sub msg


port save : ( List String, Int ) -> Cmd msg


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
        , Animation.subscription Animate <| computeAnimations model
        , svgMousePosResult SvgMousePosResult
        , Mouse.moves MousePos
        , Mouse.ups MouseUp
        ]
