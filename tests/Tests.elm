module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Matrix exposing (Matrix)
import Types exposing (Level)
import Level


all : Test
all =
    describe "Picross-elm"
        [ levelTests ]


type alias MatrixSet =
    { matrix : Matrix Bool
    , horizontalTips : List (List Int)
    , verticalTips : List (List Int)
    }


crossSet : MatrixSet
crossSet =
    { matrix =
        Matrix.fromList
            [ [ True, False, False, False, True ]
            , [ False, True, False, True, False ]
            , [ False, False, True, False, False ]
            , [ False, True, False, True, False ]
            , [ True, False, False, False, True ]
            ]
            |> Maybe.withDefault Matrix.empty
    , horizontalTips =
        [ [ 1, 1 ]
        , [ 1, 1 ]
        , [ 1 ]
        , [ 1, 1 ]
        , [ 1, 1 ]
        ]
    , verticalTips =
        [ [ 1, 1 ]
        , [ 1, 1 ]
        , [ 1 ]
        , [ 1, 1 ]
        , [ 1, 1 ]
        ]
    }


squareSet : MatrixSet
squareSet =
    { matrix =
        Matrix.fromList
            [ [ False, False, False, False, False ]
            , [ False, True, True, True, False ]
            , [ False, True, True, True, False ]
            , [ False, True, True, True, False ]
            , [ False, False, False, False, False ]
            ]
            |> Maybe.withDefault Matrix.empty
    , horizontalTips =
        [ []
        , [ 3 ]
        , [ 3 ]
        , [ 3 ]
        , []
        ]
    , verticalTips =
        [ []
        , [ 3 ]
        , [ 3 ]
        , [ 3 ]
        , []
        ]
    }


upperSet : MatrixSet
upperSet =
    { matrix =
        Matrix.fromList
            [ [ True, True, True, True, True ]
            , [ True, True, True, True, True ]
            , [ False, False, False, False, False ]
            , [ False, False, False, False, False ]
            , [ False, False, False, False, False ]
            ]
            |> Maybe.withDefault Matrix.empty
    , horizontalTips =
        [ [ 5 ]
        , [ 5 ]
        , []
        , []
        , []
        ]
    , verticalTips =
        [ [ 2 ]
        , [ 2 ]
        , [ 2 ]
        , [ 2 ]
        , [ 2 ]
        ]
    }


levelTests : Test
levelTests =
    describe "Levels"
        [ describe "getHorizontalTips"
            [ test "Cross" <|
                \() ->
                    Expect.equal
                        (Level.getHorizontalTips crossSet.matrix)
                        crossSet.horizontalTips
            , test "Square" <|
                \() ->
                    Expect.equal
                        (Level.getHorizontalTips squareSet.matrix)
                        squareSet.horizontalTips
            , test "Upper" <|
                \() ->
                    Expect.equal
                        (Level.getHorizontalTips upperSet.matrix)
                        upperSet.horizontalTips
            ]
        , describe "getVerticalTips"
            [ test "Cross" <|
                \() ->
                    Expect.equal
                        (Level.getVerticalTips crossSet.matrix)
                        crossSet.verticalTips
            , test "Square" <|
                \() ->
                    Expect.equal
                        (Level.getVerticalTips squareSet.matrix)
                        squareSet.verticalTips
            , test "Upper" <|
                \() ->
                    Expect.equal
                        (Level.getVerticalTips upperSet.matrix)
                        upperSet.verticalTips
            ]
        ]
