module Level exposing (getLevelHorizontalTips, getLevelByName)

import List.Extra
import Types exposing (Level)


getLevelHorizontalTips : Level -> List (List Int)
getLevelHorizontalTips level =
    []


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (\level -> level.name == name) levels
