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


type alias Box =
    { x : Float, y : Float, color : Color }


type alias Sketch =
    { width : Float
    , height : Float
    , frame : Int
    , boxes : List Box
    }


sketch0 =
    { width = 720
    , height = 512
    , frame = 0
    , boxes = []
    }


slug =
    """
M1112 Pencil Rain by @rsbohn.
  """


mysubs sketch =
    Sub.batch
        [ Time.every (1 * 1000) (always Tick) ]


type Msg
    = Begin Sketch
    | Tick


update msg sketch =
    case msg of
        Begin newSketch ->
            ( newSketch, Cmd.none )

        Tick ->
            ( sketch, Random.generate Begin (nextFrame sketch) )


distance ( x, y ) =
    sqrt (x * x + y * y)


colorLike : Float -> Random.Generator Color
colorLike hue =
    let
        like i =
            hsla hue 0.5 i 0.8
    in
        Random.map like (Random.float 0 0.75)


randomBoxes : Random.Generator Box
randomBoxes =
    let
        boxer x y color =
            { x = x, y = y, color = color }
    in
        Random.map3 boxer (Random.float -1 1) (Random.float -1 1) (colorLike (2 / 8 * pi))


nextFrame : Sketch -> Random.Generator Sketch
nextFrame sketch =
    let
        n =
            120

        f boxes =
            { sketch
                | frame = sketch.frame + 1
                , boxes = boxes
            }
    in
        Random.map f
            (Random.list n (randomBoxes))


backdrop sketch =
    rectangle sketch.width sketch.height
        |> solidFill (hsla (degrees 25) 0.25 0.75 1)


tview sketch =
    let
        draw box =
            rectangle 4 128
                |> solidFill box.color
                |> move (300 * box.x) (250 * box.y)
                |> rotate (cos (box.x / 2) * pi)
    in
        List.map draw sketch.boxes
            |> group


view : Sketch -> Html.Html Msg
view sketch =
    group
        [ backdrop sketch
        , M.spacer |> move (sketch.width * -0.48) 0
        , M.spacer |> move (sketch.width * 0.48) 0
        , tview sketch
        ]
        |> svg sketch.width sketch.height


viewWithMarkup sketch =
    Html.div []
        [ Html.div [] [ view sketch ]
        , Html.div []
            [ M.sourceLink "M1112.elm"
            ]
        , MD.toHtml [] slug
        ]


main =
    Html.App.program
        { init = ( sketch0, Random.generate Begin (nextFrame sketch0) )
        , update = update
        , view = viewWithMarkup
        , subscriptions = mysubs
        }
