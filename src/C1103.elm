module Main exposing (..)

import Html.App
import Graphics.Render exposing (..)
import Color exposing (hsl, hsla)
import Random


width =
    720


height =
    512


rheight =
    30


backdrop =
    rectangle width height |> solidFill (hsl (degrees 120) 0.2 0.6)


ruddy =
    hsl 0 0.95 0.8


blues =
    hsl (degrees 260) 0.95 0.4


view seed =
    group
        [ backdrop
        , rex seed ruddy |> move -140 0
        , rex seed blues |> scale 0.6
        , nameplate "C1103: Battletowers"
        ]
        |> svg width height


randomrect seed =
    let
        ( x, newSeed ) =
            Random.step (Random.int 50 150) seed
    in
        ( rectangle (toFloat x) rheight, newSeed )


rex seed col =
    let
        step n ( rects, seed ) =
            let
                ( shape, nextSeed ) =
                    randomrect seed

                rect =
                    shape |> solidFill col |> move 0 (rheight * n * 0.8)
            in
                ( rect :: rects, nextSeed )
    in
        List.foldl step ( [], Random.initialSeed seed ) [0..7]
            |> fst
            |> group


nameplate s =
    plain 24 "Arial" Color.black s |> move (-1 * width * 0.48) (height * 0.48)


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
