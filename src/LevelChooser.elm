module LevelChooser exposing (view, Model, init, mouseDown, mouseMoveWithLeftButton)

import Svg exposing (Svg, rect, g, text_, tspan)
import Svg.Attributes exposing (x, y, rx, ry, width, height, fill, transform, fontSize)
import Svg.Events
import Msg exposing (Msg(..), LevelChooserMsg(..))
import Types exposing (Level, FloatCoord)


{-| padding goes from 0.0 to 1.0
-}
type alias Model =
    { levels : List Level
    , width : Float
    , height : Float
    , padding : Float
    , verticalTilesCount : Int
    , horizontalOffset : Float
    , oldHorizontalOffset : Float
    }


init : List Level -> Float -> Float -> Model
init levels width height =
    { levels = levels
    , width = width
    , height = height
    , padding = 0.2
    , verticalTilesCount = 4
    , horizontalOffset = 0.0
    , oldHorizontalOffset = 0.0
    }


computeTileSize : Model -> Float
computeTileSize model =
    let
        vtc =
            toFloat model.verticalTilesCount
    in
        model.height / ((vtc * (1.0 + model.padding) + model.padding))


{-| Returns the number of horizontal tiles that will the entire board
-}
getHorizontalTilesSize : Model -> Int
getHorizontalTilesSize model =
    1 + ((List.length model.levels) - 1) // model.verticalTilesCount


computePaddingSize : Model -> Float
computePaddingSize model =
    (computeTileSize model) * model.padding


drawTile : Model -> Int -> Level -> Svg Msg
drawTile model index level =
    let
        padding =
            computePaddingSize model

        tileSize =
            computeTileSize model

        row =
            index % model.verticalTilesCount

        col =
            index // model.verticalTilesCount
    in
        g
            [ Svg.Attributes.cursor "pointer"
            , Svg.Events.onMouseUp <| LevelChooserMsg <| MouseUpOnTile <| level.name
            ]
            [ rect
                [ x <| toString <| padding + (toFloat col) * (padding + tileSize)
                , y <| toString <| padding + (toFloat row) * (padding + tileSize)
                , rx "8.0"
                , rx "8.0"
                , width <| toString tileSize
                , height <| toString tileSize
                , fill "blue"
                ]
                []
            , text_
                [ x <| toString <| padding + (toFloat col) * (padding + tileSize) + tileSize / 2.0
                , y <| toString <| padding + (toFloat row) * (padding + tileSize) + tileSize / 2.0
                , fill "white"
                , fontSize <| toString (tileSize / 4.0)
                ]
                [ tspan
                    [ Svg.Attributes.dominantBaseline "central"
                    , Svg.Attributes.textAnchor "middle"
                    ]
                    [ Svg.text <| getLevelSizeText level ]
                ]
            ]


getLevelSizeText : Level -> String
getLevelSizeText level =
    let
        rowsCount =
            List.length level.content

        firstCol =
            Maybe.withDefault [] <| List.head level.content

        colsCount =
            List.length firstCol
    in
        (toString colsCount) ++ "x" ++ (toString rowsCount)


drawTiles : Model -> Svg Msg
drawTiles model =
    model.levels
        |> List.indexedMap (drawTile model)
        |> g
            [ transform <| "translate(" ++ (toString model.horizontalOffset) ++ ")" ]


view : Model -> List (Svg Msg)
view model =
    List.concat
        [ [ rect
                [ x <| "0.0"
                , y <| "0.0"
                , Svg.Attributes.width <| toString model.width
                , Svg.Attributes.height <| toString model.height
                , Svg.Attributes.fill "lightblue"
                ]
                []
          ]
        , [ drawTiles model ]
        ]


mouseDown : Model -> Model
mouseDown model =
    { model | oldHorizontalOffset = model.horizontalOffset }


mouseMoveWithLeftButton : FloatCoord -> FloatCoord -> Model -> Model
mouseMoveWithLeftButton downPos currentPos model =
    let
        diff =
            currentPos.x - downPos.x

        projectedOffset =
            model.oldHorizontalOffset + diff

        padding =
            computePaddingSize model

        boardWidth =
            (toFloat <| (getHorizontalTilesSize model)) * (computeTileSize model + padding) + padding

        newOffset =
            if projectedOffset > 0 then
                0
            else if projectedOffset < -(boardWidth - model.width) then
                -(boardWidth - model.width)
            else
                projectedOffset
    in
        { model
            | horizontalOffset = newOffset
        }
