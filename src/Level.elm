module Level
    exposing
        ( getHorizontalTips
        , getVerticalTips
        , getLevelByName
        )

import List.Extra
import Matrix exposing (Matrix)
import Array exposing (Array)
import Types exposing (Level)


boolToInt : Bool -> Int
boolToInt b =
    case b of
        True ->
            1

        False ->
            0


getBoolArrayTips : List Bool -> List Int
getBoolArrayTips row =
    row
        |> List.map boolToInt
        |> List.Extra.group
        |> List.map List.sum
        |> List.filter (flip (>) 0)


getHorizontalTips : Matrix Bool -> List (List Int)
getHorizontalTips matrix =
    let
        rows : List (Array Bool)
        rows =
            List.range 0 (Matrix.height matrix - 1)
                |> List.filterMap (\rowIndex -> Matrix.getRow rowIndex matrix)
    in
        List.map (Array.toList >> getBoolArrayTips) rows


getVerticalTips : Matrix Bool -> List (List Int)
getVerticalTips matrix =
    let
        columns : List (Array Bool)
        columns =
            List.range 0 (Matrix.width matrix - 1)
                |> List.filterMap (\colIndex -> Matrix.getColumn colIndex matrix)
    in
        List.map (Array.toList >> getBoolArrayTips) columns


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (.name >> (==) name) levels
