module M1112 exposing (..)

import Bitwise exposing (and, shiftLeft)
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


type alias Sketch =
    { width : Float
    , height : Float
    , registers : List Int
    , delta : Float
    , frame : Int
    }


sketch0 =
    { width = 720
    , height = 512
    , registers = []
    , delta = 0.0
    , frame = 0
    }


slug =
    """
M1112 Something Awesome by @rsbohn.
  """


mysubs sketch =
    Sub.batch
        [ Time.every (1000 / 4) (always Tick) ]


type Msg
    = Begin Sketch
    | Tick


update msg sketch =
    case msg of
        Begin newSketch ->
            ( newSketch, Cmd.none )

        Tick ->
            ( sketch, Random.generate Begin (nextState sketch) )


nextState : Sketch -> Random.Generator Sketch
nextState sketch =
    let
        f xs delta =
            { sketch
                | registers = xs
                , delta = delta
                , frame = sketch.frame + 1
            }
    in
        Random.map2 f (Random.list 8 (Random.int 30 90)) (Random.float -200 200)


backdrop sketch =
    rectangle sketch.width sketch.height
        |> solidFill (hsla (degrees 25) 0.25 0.75 1)


tower sketch =
    let
        r =
            40

        dx =
            r * (cos ((toFloat sketch.frame) / 32))

        dy =
            r * (sin ((toFloat sketch.frame) / 32))

        rects i w =
            rectangle (toFloat w) 30
                |> solidFill Color.blue
                |> move 0 (27 * (toFloat i))
    in
        List.indexedMap rects sketch.registers
            |> group
            |> move dx dy


view : Sketch -> Html.Html Msg
view sketch =
    group
        [ backdrop sketch
        , M.spacer |> move (sketch.width * -0.48) 0
        , M.spacer |> move (sketch.width * 0.48) 0
        , tower sketch
        , rectangle (sketch.width - 80) 120
            |> solidFillWithBorder (hsla (pi / 18) 1 0.8 0.4) 4 Color.charcoal
        ]
        |> svg sketch.width sketch.height


viewWithMarkup sketch =
    Html.div []
        [ Html.div [] [ view sketch ]
        , Html.div []
            [ M.sourceLink "M1112.elm"
            ]
        , MD.toHtml [] slug
          --, Html.text (toString ((toFloat sketch.frame) * pi / 32))
        ]


main =
    Html.App.program
        { init = ( sketch0, Random.generate Begin (nextState sketch0) )
        , update = update
        , view = viewWithMarkup
        , subscriptions = mysubs
        }
