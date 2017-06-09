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
        , selectionToArea
        )


type alias Coord =
    { x : Int
    , y : Int
    }


type alias GridSize =
    { width : Int
    , height : Int
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
    { fixed : GridCoord
    , floating : GridCoord
    }


type alias CellArea =
    { topLeft : GridCoord
    , size : GridSize
    }


type alias Level =
    { name : String
    , uuid : String
    , description : String
    , content : List (List Bool)
    }


type MouseButton
    = LeftButton
    | RightButton


selectionToArea : CellSelection -> CellArea
selectionToArea selection =
    let
        ( col1, col2 ) =
            if selection.fixed.col <= selection.floating.col then
                ( selection.fixed.col, selection.floating.col )
            else
                ( selection.floating.col, selection.fixed.col )

        ( row1, row2 ) =
            if selection.fixed.row <= selection.floating.row then
                ( selection.fixed.row, selection.floating.row )
            else
                ( selection.floating.row, selection.fixed.row )
    in
        { topLeft = { col = col1, row = row1 }
        , size = { width = col2 - col1 + 1, height = row2 - row1 + 1 }
        }
