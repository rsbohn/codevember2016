module M1110 exposing (..)

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


type LampState
    = Off
    | On


type alias Sketch =
    { width : Float
    , height : Float
    , registers : List Int
    , meter0 : Float
    , meter1 : Float
    }


sketch0 =
    { width = 720
    , height = 512
    , registers = [ 0, 192 ]
    , meter0 = 0.0
    , meter1 = 0.0
    }


slug =
    """
M1110 Blinkenlights by @rsbohn.
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


getLampState : Bool -> LampState
getLampState a =
    if a then
        On
    else
        Off


nextState : Sketch -> Random.Generator Sketch
nextState sketch =
    let
        f xs =
            { sketch | registers = xs }
    in
        Random.map f (Random.list 8 (Random.int 0 255))


backdrop sketch =
    rectangle sketch.width sketch.height
        |> solidFill (hsla (degrees 25) 0.25 0.75 1)


panel state =
    let
        glassColor =
            case state of
                On ->
                    hsla (degrees 8) 1 0.8 0.4

                Off ->
                    hsla (degrees 8) 1 0.2 0.4
    in
        rectangle 60 40 |> solidFillWithBorder glassColor 5 Color.charcoal


register : Int -> Form b
register c8 =
    let
        decode n =
            if (and c8 (shiftLeft 1 n)) > 0 then
                On
            else
                Off

        spread n items =
            List.indexedMap
                (\i k ->
                    move (toFloat (i * n)) 0 k
                )
                items
    in
        [0..7]
            |> List.reverse
            |> List.map decode
            |> List.map panel
            |> spread 70
            |> group


rview : List Int -> List (Form b)
rview rs =
    List.map register rs
        |> List.indexedMap
            (\i k ->
                move 0
                    (toFloat (i * 48))
                    k
            )


view : Sketch -> Html.Html Msg
view sketch =
    group
        [ backdrop sketch
        , M.spacer |> move (sketch.width * -0.48) 0
        , M.spacer |> move (sketch.width * 0.48) 0
        , rview sketch.registers
            |> group
            |> move (sketch.width * -0.36) -200
        ]
        |> svg sketch.width sketch.height


viewWithMarkup sketch =
    Html.div []
        [ Html.div [] [ view sketch ]
        , Html.div []
            [ M.sourceLink "M1110.elm"
            ]
        , MD.toHtml [] slug
        ]


main =
    Html.App.program
        { init = ( sketch0, Random.generate Begin (nextState sketch0) )
        , update = update
        , view = viewWithMarkup
        , subscriptions = mysubs
        }
