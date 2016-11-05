module C1104 exposing (..)

import Html.App
import Graphics.Render exposing (..)
import Color exposing (hsl, hsla)
import Random


width =
    720


height =
    512


backdrop =
    rectangle width height |> solidFill (hsl (degrees 8) 0.15 0.6)


randomAngle : Float -> Float -> Random.Generator Float
randomAngle a b =
    Random.map degrees (Random.float a b)


colorLike : Color.Color -> Random.Generator Color.Color
colorLike ref =
    let
        base =
            hsl (Color.toHsl ref).hue
    in
        Random.map2 base (Random.float 0.4 0.8) (Random.float 0.2 0.8)


translucent : Color.Color -> Color.Color
translucent c =
    let
        original =
            Color.toHsl c
    in
        hsla original.hue original.saturation original.lightness 0.8


core r col seed =
    let
        ( mycolor, seed1 ) =
            Random.step (colorLike col) seed

        ( shift, seed2 ) =
            Random.step (Random.float -1 1) seed1

        form =
            ellipse r r
                |> solidFill (translucent mycolor)
                |> move 0 (r * shift / 10.0)
    in
        ( form, seed2 )


rose color0 seed =
    let
        step n ( circs, seed ) =
            let
                ( circ, newSeed ) =
                    core (n * 30) color0 seed
            in
                ( circ :: circs, newSeed )
    in
        List.foldl step ( [], seed ) [1..12]
            |> fst
            |> group


view base =
    let
        seed =
            Random.initialSeed base
    in
        group
            [ backdrop
            , rose (hsl (degrees 12) 0.5 0.5) seed |> move -180 -40
            , nameplate "C1104: Iberian Rose"
            ]
            |> svg width height


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
