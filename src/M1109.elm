module M1109 exposing (..)

import Color exposing (Color, hsla, toHsl)
import Graphics.Render exposing (..)
import Html
import Html.App
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Markdown as MD
import Random
import Time
import M1107 as M


sketch =
    { width = 720, height = 512 }


about frame theta =
    (toFloat (frame % 120)) + theta


wires r =
    [ (M.rplot r -90)
    , (M.rplot r 30)
    , (M.rplot r 150)
    ]


transparent =
    Color.hsla 0 0 0 0


backdrop =
    rectangle sketch.width sketch.height |> solidFill (hsla (degrees 25) 0.25 0.75 1)


view model =
    Html.div [ onClick OtherColor ]
        [ group
            [ backdrop
            , M.spacer |> move (-1 * sketch.width * 0.48) 0
            , M.spacer |> move (1 * sketch.width * 0.48) 0
            , ellipse 78 78 |> solidFill model.color
            , ellipse 80 80 |> solidFill Color.green |> M.rmove 160 (about model.frame 150)
            , ellipse 80 80 |> solidFill Color.green |> M.rmove 160 (about model.frame -90)
            , ellipse 80 80 |> solidFill Color.green |> M.rmove 160 (about model.frame 30)
            , polygon (wires 160)
                |> solidFillWithBorder transparent 5 Color.charcoal
                |> rotate (degrees (about model.frame 0))
            , M.nameplate sketch "M1109: Click and Rotate"
            ]
            |> svg sketch.width sketch.height
        , Html.div [] [ M.sourceLink "M1109.elm" ]
        , MD.toHtml [ HA.class "bumper" ] bumper
        ]


bumper =
    """
Just some spinning balls. View has `onclick` event handler.

@rsbohn #codevember
"""


type Msg
    = Tick
    | Splash Color
    | OtherColor


colorLike : Color -> Random.Generator Color
colorLike c =
    let
        like q =
            hsla (toHsl c).hue 1 q 0.8
    in
        Random.map like (Random.float 0 1)


update msg model =
    case msg of
        Tick ->
            ( { model | frame = model.frame + 1 }, Cmd.none )

        Splash c ->
            ( { model | color = c }, Cmd.none )

        OtherColor ->
            ( model, Random.generate Splash (colorLike Color.red) )


model0 =
    { frame = 0, color = Color.yellow }


main =
    Html.App.program
        { init = ( model0, Random.generate Splash (colorLike Color.red) )
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every (1000 / 30) (always Tick)
        }
