module C1105 exposing (..)

-- This is a library module for the rest of the month

import Html exposing (text)
import Html.App
import Html.Attributes
import Color exposing (hsl, hsla)
import Graphics.Render exposing (..)
import Random as R
import Time


baseUrl =
    "https://github.com/rsbohn/codevember2016"


width =
    720


height =
    512


backdrop =
    rectangle width height |> solidFill (hsl (degrees 200) 0.15 0.6)


randomAngle : Float -> Float -> R.Generator Float
randomAngle a b =
    R.map degrees (R.float a b)


colorLike : Color.Color -> R.Generator Color.Color
colorLike ref =
    let
        base =
            hsl (Color.toHsl ref).hue
    in
        R.map2 base (R.float 0.4 0.8) (R.float 0.2 0.8)


translucent : Color.Color -> Color.Color
translucent c =
    let
        original =
            Color.toHsl c
    in
        hsla original.hue original.saturation original.lightness 0.3


angle =
    R.step (randomAngle 1 10) (R.initialSeed 0) |> fst


nameplate s =
    plain 24 "Source Sans Pro" Color.black s
        |> move (-1 * width * 0.48) (height * 0.48)


monitor s =
    plain 24 "Source Sans Pro" Color.black s
        |> move (-1 * width * 0.48) (height * 0.42)


cubic x =
    x * x * x


at x y =
    move (10 * x) (-0.4 * y)


marker ( x, y ) =
    ellipse 2 2 |> solidFill Color.black |> at x y


sketch =
    let
        col =
            Color.darkBlue

        step a b =
            ( a, (cubic a / 10) ) :: b
    in
        [-20..20]
            |> List.foldl step []
            |> List.map marker
            |> group


spark x =
    let
        xx =
            x - 20

        yy =
            cubic xx / 10
    in
        [ rectangle 24 (height * 0.96) |> solidFill (translucent Color.red) |> at xx 0
        , rectangle (width * 0.88) 24 |> solidFill (translucent Color.blue) |> at 0 yy
        , rectangle 12 12 |> solidFill Color.white |> at xx yy
        , ellipse 4 4 |> solidFill Color.black |> at xx yy
        ]
            |> group


type alias Model =
    Int


type Msg
    = Tick


view model =
    group
        [ backdrop
        , sketch
        , spark model
          --, monitor (toString (model - 20))
        , nameplate "C1105 Carpeted Garage"
        ]
        |> svg width height
        |> withSourceLink


withSourceLink graph =
    Html.div []
        [ Html.div [] [ graph ]
        , Html.div []
            [ Html.a [ Html.Attributes.href (baseUrl ++ "/blob/master/src/C1105.elm") ]
                [ Html.text "source" ]
            ]
        ]


main =
    Html.App.program
        { init = ( 1, Cmd.none )
        , update = \msg model -> ( (model + 1) % 40, Cmd.none )
        , view = view
        , subscriptions = \_ -> Time.every 500 (always Tick)
        }
