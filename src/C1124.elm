module C1124 exposing (..)

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
    ellipse 10 10
        |> solidFill (hsla (hue * pi / 6) 1 0.5 0.2)
        |> move x y


rotx theta =
    let
        t =
            toFloat theta
    in
        rotate (t * pi / 60)


cyclic m theta =
    let
        r =
            2 + 2 * cos (2 * theta) * m

        x =
            r * cos theta

        y =
            r * sin theta
    in
        ( x, y )


sketch model =
    let
        m =
            80 - (sin (toFloat model.frame / 8)) * 10

        n =
            64

        figure i =
            box 1 <| cyclic m (pi * i / n)
    in
        List.map figure [-n..n]
            |> group
            |> rotx model.frame


viewSketch : Model -> Html Msg
viewSketch model =
    [ ellipse 180 180
        |> solidFillWithBorder (hsla 0 0 0 0)
            2
            (hsla 0 0 0.2 1)
    , sketch model
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
        , Html.div [ tstyle ] [ Html.text "Codevember C1124" ]
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
