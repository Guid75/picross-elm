module Level exposing (getLevelByName)

import List.Extra
import Types exposing (Level)


getLevelByName : String -> List Level -> Maybe Level
getLevelByName name levels =
    List.Extra.find (.name >> (==) name) levels
