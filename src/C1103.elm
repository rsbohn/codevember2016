module Main exposing (..)

import Html.App
import Graphics.Render exposing (..)
import Color exposing (hsl, hsla)
import Random


width =
    720


height =
    512


backdrop =
    rectangle width height |> solidFill (hsl (degrees 120) 0.2 0.6)


ruddy =
    hsl 0 0.95 0.8


blues =
    hsl (degrees 260) 0.95 0.4


view seed =
    group
        [ backdrop
        , rex seed -200 ruddy
        , rex seed 50 blues
        ]
        |> svg width height


spread items h n =
    n * h / items - h / 2


randomrect seed =
    let
        ( x, newSeed ) =
            Random.step (Random.int 50 150) seed
    in
        ( rectangle (toFloat x) 30, newSeed )


ubica x q =
    move x (spread 5 (height / 4) q)


rex seed xbase col =
    let
        step n ( rects, seed ) =
            let
                ( shape, nextSeed ) =
                    randomrect seed

                rect =
                    shape |> solidFill col |> ubica xbase n
            in
                ( rect :: rects, nextSeed )
    in
        List.foldl step ( [], Random.initialSeed seed ) [0..7]
            |> (\( rs, _ ) -> rs)
            |> group


type alias Model =
    Int


update msg model =
    msg


main =
    Html.App.program
        { init = ( 1, Random.generate identity (Random.int 0 255) )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }
