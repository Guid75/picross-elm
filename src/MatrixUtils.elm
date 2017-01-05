module MatrixUtils
    exposing
        ( getHorizontalTips
        , getVerticalTips
        )

import List.Extra
import Matrix exposing (Matrix)
import Array exposing (Array)


{-| False gives 0 and True gives 1
-}
boolToInt : Bool -> Int
boolToInt b =
    case b of
        True ->
            1

        False ->
            0


{-| If the given list is empty, add at least a 0 in it
-}
withDefaultZero : List Int -> List Int
withDefaultZero list =
    if List.isEmpty list then
        [ 0 ]
    else
        list


{-| This is the core function of the tips system, it takes
a sequence of boolean values and returns all consecutive true sequences lengthes
in it. For instance [true, true, false, true, true true] returns [2, 3]
-}
getBoolArrayTips : List Bool -> List Int
getBoolArrayTips row =
    row
        |> List.map boolToInt
        |> List.Extra.group
        |> List.map List.sum
        |> List.filter (flip (>) 0)
        |> withDefaultZero


{-| Returns a list of all rows arrays of the given matrix
-}
getRows : Matrix a -> List (Array a)
getRows matrix =
    List.range 0 (Matrix.height matrix - 1)
        |> List.filterMap (flip Matrix.getRow matrix)


{-| Returns a list of all columns arrays of the given matrix
-}
getColumns : Matrix a -> List (Array a)
getColumns matrix =
    List.range 0 (Matrix.width matrix - 1)
        |> List.filterMap (flip Matrix.getColumn matrix)


getHorizontalTips : Matrix Bool -> List (List Int)
getHorizontalTips matrix =
    List.map (Array.toList >> getBoolArrayTips) <| getRows matrix


getVerticalTips : Matrix Bool -> List (List Int)
getVerticalTips matrix =
    List.map (Array.toList >> getBoolArrayTips) <| getColumns matrix
