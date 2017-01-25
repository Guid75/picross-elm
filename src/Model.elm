module Model
    exposing
        ( Model
        , init
        , State(..)
        , AnimState(..)
        , ShrinkAnims
        , ShrinkAnim
        )

import Dict exposing (Dict)
import Matrix exposing (Matrix)
import Animation.Messenger
import Msg exposing (Msg)
import Types exposing (Level, CellSelection, FloatCoord, GridCoord, Cell, MouseButton, CellType(..))
import Grid exposing (Grid)
import LevelChooser


type State
    = Init
    | ChoosingLevel LevelChooser.Model
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
    , mouseButtonDown : Maybe MouseButton
    , currentSvgMousePos : Maybe FloatCoord
    , downSvgMousePos : Maybe FloatCoord
    , state : State
    , grid : Grid
    , hoveredCell : Maybe GridCoord
    , selection : Maybe CellSelection
    , levels : Maybe (List Level)
    , currentLevel : Maybe String
    , boundingBox : { x : Float, y : Float, width : Float, height : Float }
    }


init : Model
init =
    { board =
        Matrix.repeat 17 20 (Cell Empty False)
    , state = Init
    , mouseButtonDown = Nothing
    , currentSvgMousePos = Nothing
    , downSvgMousePos = Nothing
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


gridColor : String
gridColor =
    "black"
