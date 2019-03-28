module Cell exposing (Cell(..), decoder)

import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (required)
import Point as Point exposing (Point)


type Cell
    = Cell (Maybe Point) (Maybe Point) (Maybe Point) (Maybe Point)


decoder : Decoder Cell
decoder =
    Decode.succeed Cell
        |> required "up" (nullable Point.decoder)
        |> required "right" (nullable Point.decoder)
        |> required "down" (nullable Point.decoder)
        |> required "left" (nullable Point.decoder)
