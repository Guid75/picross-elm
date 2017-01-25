module Msg exposing (Msg(..), LevelChooserMsg(..))

import Http
import Animation
import Animation.Messenger
import Types exposing (Level, Coord)


type Msg
    = NoOp
    | BoardMouseDown Int
    | BoardSizeResult ( Float, Float, Float, Float )
    | GetLevels (Result Http.Error (List Level))
    | Cheat
    | GoToLevelChooser
    | ChoseLevel String
    | Animate Animation.Msg
    | SvgMouseMove Coord
    | SvgMouseLeave
    | TransMousePosResult ( Float, Float )
    | TransMousePosResult2 ( Float, Float )
    | EndOfFade
    | LevelChooserMsg LevelChooserMsg
    | MousePos { x : Int, y : Int }
    | MouseUp { x : Int, y : Int }

type LevelChooserMsg
    = ClickOnTile String