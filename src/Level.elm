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


withDefaultZero : List Int -> List Int
withDefaultZero list =
    if List.isEmpty list then
        [ 0 ]
    else
        list


getBoolArrayTips : List Bool -> List Int
getBoolArrayTips row =
    row
        |> List.map boolToInt
        |> List.Extra.group
        |> List.map List.sum
        |> List.filter (flip (>) 0)
        |> withDefaultZero


getRows : Matrix Bool -> List (Array Bool)
getRows matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.filterMap (flip Matrix.getRow matrix)


getColumns : Matrix Bool -> List (Array Bool)
getColumns matrix =
    List.range 0 (Matrix.width matrix - 1)
        |> List.filterMap (flip Matrix.getColumn matrix)


getHorizontalTips : Matrix Bool -> List (List Int)
getHorizontalTips matrix =
    List.map (Array.toList >> getBoolArrayTips) <| getRows matrix


getVerticalTips : Matrix Bool -> List (List Int)
getVerticalTips matrix =
    List.map (Array.toList >> getBoolArrayTips) <| getColumns matrix


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (.name >> (==) name) levels
