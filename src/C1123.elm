module C1123 exposing (..)

import Color exposing (Color, hsla)
import Css
import Html exposing (Html)
import Html.App
import Html.Attributes as HA
import Graphics.Render exposing (..)
import Random
import Time


type Msg
    = Tick


type alias Model =
    { frame : Int }


m0 =
    { frame = 0 }


update msg model =
    case msg of
        Tick ->
            ( { model | frame = model.frame + 1 }, Cmd.none )


box hue ( x, y ) =
    rectangle 80 80
        |> solidFill (hsla (hue * pi / 6) 1 0.5 0.2)
        |> move x y


rotx theta =
    let
        t =
            toFloat theta
    in
        rotate (t * pi / 60)


viewSketch : Model -> Html Msg
viewSketch model =
    [ ellipse 180 180
        |> solidFillWithBorder (hsla 0 0 0 0)
            2
            (hsla 0 0 0.2 1)
    , [-4..4]
        |> List.indexedMap (\n p -> box 1 ( p * 42, (sin (toFloat (n + model.frame) / 4)) * 120 ))
        |> group
      --    , List.map (box 1) model.points |> group |> rotx model.rotation
      --    , List.map (box 2) model.points |> group |> rotx (40 - model.rotation)
    ]
        |> group
        |> svg 720 512


tstyle =
    [ Css.fontSize (Css.px 32)
    , Css.width (Css.px 720)
    , Css.textAlign Css.center
    ]
        |> Css.asPairs
        |> HA.style


view model =
    Html.div []
        [ Html.div [] [ (viewSketch model) ]
        , Html.div [ tstyle ] [ Html.text "Codevember C1123" ]
        ]


mysubs model =
    Time.every (1000 / 12) (always Tick)


main =
    Html.App.program
        { init = ( m0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = mysubs
        }
