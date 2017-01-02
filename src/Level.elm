module Level exposing (getHorizontalTips, getVerticalTips, getLevelByName)

import List.Extra
import Matrix exposing (Matrix)
import Array exposing (Array)
import Types exposing (Level)


getBoolArrayTips : Array Bool -> List Int
getBoolArrayTips row =
    row
        |> Array.toList
        |> List.Extra.group
        |> List.filter (\group -> List.head group == Just True)
        |> List.map List.length


getHorizontalTips : Matrix Bool -> List (List Int)
getHorizontalTips matrix =
    let
        rows : List (Array Bool)
        rows =
            List.range 0 (Matrix.height matrix - 1)
                |> List.filterMap (\rowIndex -> Matrix.getRow rowIndex matrix)
    in
        List.map getBoolArrayTips rows


getVerticalTips : Matrix Bool -> List (List Int)
getVerticalTips matrix =
    let
        columns : List (Array Bool)
        columns =
            List.range 0 (Matrix.width matrix - 1)
                |> List.filterMap (\colIndex -> Matrix.getColumn colIndex matrix)
    in
        List.map getBoolArrayTips columns


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (\level -> level.name == name) levels
