module Types
    exposing
        ( GridCoord
        , FloatCoord
        , Coord
        , CellType(..)
        , Cell
        , CellSelection
        , Level
        , MouseButton(..)
        )


type alias Coord =
    { x : Int
    , y : Int
    }


type alias GridCoord =
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
    { firstCell : GridCoord
    , lastCell : GridCoord
    }


type alias Level =
    { name : String
    , description : String
    , content : List (List Bool)
    }


type MouseButton
    = LeftButton
    | RightButton
