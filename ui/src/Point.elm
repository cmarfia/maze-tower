module Point exposing (Point(..), decoder)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)


type Point
    = Point Int Int


decoder : Decoder Point
decoder =
    Decode.succeed Point
        |> required "row" int
        |> required "col" int
