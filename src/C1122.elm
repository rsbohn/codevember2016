module C1122 exposing (..)

-- for my brother
-- on his birthday

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
    | MorePoints (List (Point))


type alias Model =
    { hue : Float, rotation : Int, points : List Point }


m0 =
    { hue = 0.0, rotation = 0, points = [ ( 0, 20 ) ] }


update msg model =
    case msg of
        Tick ->
            ( { model | rotation = model.rotation + 1 }, Cmd.none )

        MorePoints ps ->
            ( { model | points = ps }, Cmd.none )


box hue ( x, y ) =
    let
        hue_ =
            toFloat (hue % 6)
    in
        rectangle 80 80
            |> solidFill (hsla (hue_ * pi / 3) 1 0.5 0.2)
            |> move x y


rotx theta =
    let
        t =
            toFloat theta
    in
        rotate (t * pi / 60)


viewSketch : Model -> Html Msg
viewSketch model =
    [ ellipse 180 180 |> solidFillWithBorder (hsla 0 0 0 0) 2 (hsla 0 0 0.2 1)
    , List.map (box 1) model.points |> group |> rotx model.rotation
    , List.map (box 2) model.points |> group |> rotx (40 + model.rotation)
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
        , Html.div [ tstyle ] [ Html.text "Happy Birthday Tracy! 2016-11-22" ]
        ]


randomPoints : Random.Generator (List (Point))
randomPoints =
    Random.list
        12
        (Random.pair
            (Random.float -120 120)
            (Random.float -120 120)
        )


mysubs model =
    Time.every (250) (always Tick)


main =
    Html.App.program
        { init = ( m0, Random.generate MorePoints randomPoints )
        , update = update
        , view = view
        , subscriptions = mysubs
        }
