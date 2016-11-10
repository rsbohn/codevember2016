module M1110 exposing (..)

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
    , lamp0 : LampState
    , lamp1 : LampState
    , meter0 : Float
    , meter1 : Float
    }


sketch0 =
    { width = 720
    , height = 512
    , lamp0 = Off
    , lamp1 = Off
    , meter0 = 0.0
    , meter1 = 0.0
    }


slug =
    """
This is a thing.
  """


mysubs sketch =
    Sub.none


type Msg
    = Begin Sketch


update msg sketch =
    case msg of
        Begin newSketch ->
            ( newSketch, Cmd.none )


getLampState : Bool -> LampState
getLampState a =
    if a then
        On
    else
        Off


state0 : Sketch -> Random.Generator Sketch
state0 sketch =
    let
        f a b =
            { sketch | lamp0 = getLampState a, lamp1 = getLampState b }
    in
        Random.map2 f Random.bool Random.bool


backdrop sketch =
    rectangle sketch.width sketch.height
        |> solidFill (hsla (degrees 25) 0.25 0.75 1)


panel state =
    let
        glassColor =
            case state of
                On ->
                    hsla (degrees 30) 1 0.8 0.2

                Off ->
                    hsla (degrees 30) 1 0.2 0.2
    in
        rectangle 120 80 |> solidFillWithBorder glassColor 5 Color.charcoal


view : Sketch -> Html.Html Msg
view sketch =
    group
        [ backdrop sketch
        , panel sketch.lamp0 |> move -100 0
        , panel sketch.lamp1 |> move 100 0
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
        { init = ( sketch0, Random.generate Begin (state0 sketch0) )
        , update = update
        , view = viewWithMarkup
        , subscriptions = mysubs
        }
