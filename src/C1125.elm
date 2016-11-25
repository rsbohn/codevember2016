module C1125 exposing (..)

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
    ellipse 80 80
        |> solidFill (hsla (hue * pi / 6) 0.6 0.2 0.6)
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
            80 - (sin (toFloat model.frame / 16)) * 20

        n =
            8

        figure i =
            box 3 <| cyclic m (pi * i / n)
    in
        List.map figure [-n..n]
            |> group
            |> rotx model.frame


viewSketch25 : Model -> Html Msg
viewSketch25 model =
    [ ellipse 180 180
        |> solidFillWithBorder (hsla 0 1 0.25 1)
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
        [ Html.div [] [ (viewSketch25 model) ]
        , Html.div [ tstyle ] [ Html.text "Codevember C1125" ]
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
