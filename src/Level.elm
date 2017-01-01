module Level exposing (getLevelHorizontalTips, getLevelByName)

import List.Extra
import Types exposing (Level)


getLevelRowHorizontalTips : List Bool -> List Int
getLevelRowHorizontalTips row =
    row
        |> List.Extra.group
        |> List.filter (\group -> List.head group == Just True)
        |> List.map List.length


getLevelHorizontalTips : Level -> List (List Int)
getLevelHorizontalTips level =
    List.map getLevelRowHorizontalTips level.content


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (\level -> level.name == name) levels
