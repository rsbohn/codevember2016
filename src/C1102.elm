module Main exposing (..)

-- after https://github.com/avh4-experimental/elm-graphics
-- also https://github.com/avh4/codevember-2016

import Graphics.Render exposing (..)
import Color exposing (rgb, hsl, hsla)


width =
    720


height =
    512


background =
    rectangle width height |> solidFill (hsl (degrees 120) 0.5 0.95)


translate : Float -> Float -> Form msg -> Form msg
translate radius angle =
    move (radius * (cos angle))
        (radius * (sin angle))


ray radius theta =
    segment ( 0, 0 ) ( radius * cos theta, radius * sin theta )


my_circle radius theta col =
    ellipse 60 60 |> solidFill col |> translate radius theta


main =
    group
        [ background
        , [0..360 / 15]
            |> List.map (\n -> ray 140 (degrees n * 15) |> solid 3 (rgb 128 128 128))
            |> group
        , [0..360 / 15]
            |> List.map
                (\n ->
                    my_circle 180 (degrees n * 15) (hsla (degrees n * 15) 0.8 0.6 0.2)
                )
            |> group
        , ellipse 10 10 |> solidFill (hsl (degrees 240) 0.8 0.2)
        ]
        |> svg width height
