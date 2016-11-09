module C1108 exposing (..)

import Color exposing (Color, hsl, hsla)
import Graphics.Render exposing (..)
import Html
import Html.App
import Time


sketch =
    { width = 720, height = 512 }


rplot ( r, theta ) =
    ( r * (cos (degrees theta)), r * (sin (degrees theta)) )


star r =
    [0..9]
        |> List.map (\i -> ( toFloat (r * (i % 2 + 1)), (i * 36) ))
        |> List.map rplot
        |> polygon


starColor =
    hsla (pi / 5) 0.5 0.5 0.5


message fontSize s =
    plain fontSize "Source Sans Pro" Color.black s


bar =
    120


mview : Model -> Html.Html a
mview model =
    group
        [ [ star 20
                |> solidFill Color.red
                |> rotate (degrees 4 * -(toFloat model.frame))
                |> move -bar 0
          , star 20
                |> solidFill Color.blue
                |> rotate (degrees 4 * -(toFloat model.frame))
                |> move bar 0
          ]
            |> group
            |> rotate (degrees 4 * (toFloat model.frame))
        , message 120 "I voted!" |> move -180 40
        , message 24 "C1108: U.S. Election 2016" |> move (-1 * sketch.width * 0.48) 240
        ]
        |> svg sketch.width sketch.height


type Msg
    = Tick


type alias Model =
    { frame : Int }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( { model | frame = model.frame + 1 % 90 }, Cmd.none )


main =
    Html.App.program
        { init = ( { frame = 0 }, Cmd.none )
        , update = update
        , view = mview
        , subscriptions = \_ -> Time.every (1000 / 30) (always Tick)
        }



--Html.text (toString (star 20))
