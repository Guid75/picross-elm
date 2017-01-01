module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Types exposing (Level)
import Level


all : Test
all =
    describe "Picross-elm"
        [ levelTests ]


crossLevel : Level
crossLevel =
    { name = "cross"
    , description = "a level with a cross form"
    , content =
        [ [ True, False, True, False, True ]
        , [ False, True, False, True, False ]
        , [ False, False, True, False, False ]
        , [ False, True, False, True, False ]
        , [ True, False, True, False, True ]
        ]
    }


levelTests : Test
levelTests =
    describe "Levels"
        [ describe "getLevelHorizontalTips"
            [ test "Foo" <|
                \() ->
                    Expect.equal
                        (Level.getLevelHorizontalTips crossLevel)
                        [ [ 1, 1, 1 ]
                        , [ 1, 1 ]
                        , [ 1 ]
                        , [ 1, 1 ]
                        , [ 1, 1, 1 ]
                        ]
            ]
        ]
