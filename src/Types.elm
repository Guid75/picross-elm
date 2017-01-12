module Types
    exposing
        ( Coord
        , FloatCoord
        , CellType(..)
        , Cell
        , CellSelection
        , Level
        )


type alias Coord =
    { col : Int
    , row : Int
    }


type alias FloatCoord =
    { x : Float
    , y : Float
    }


type CellType
    = Selected
    | Rejected
    | Empty


type alias Cell =
    { userChoice : CellType
    , value : Bool
    }


type alias CellSelection =
    { firstCell : Coord
    , lastCell : Coord
    }


type alias Level =
    { name : String
    , description : String
    , content : List (List Bool)
    }
