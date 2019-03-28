module Maze exposing (Maze(..), decoder)

import Cell as Cell exposing (Cell)
import Json.Decode as Decode exposing (Decoder, int, list)
import Json.Decode.Pipeline exposing (required)
import Point as Point exposing (Point)


type Maze
    = Maze Point Point (List (List Cell))


decoder : Decoder Maze
decoder =
    Decode.succeed Maze
        |> required "entrance" Point.decoder
        |> required "exit" Point.decoder
        |> required "maze" mazeDecoder


mazeDecoder : Decoder (List (List Cell))
mazeDecoder =
    list rowDecoder


rowDecoder : Decoder (List Cell)
rowDecoder =
    list Cell.decoder
