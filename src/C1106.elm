module C1106 exposing (..)

-- Use resources from C1105
-- Make a cycling List
-- Fill it with colors
-- Use the colors to paint a series of shapes

import C1105 as CV
import Color exposing (hsl)
import Graphics.Render exposing (..)
import Html
import Html.App
import Html.Events exposing (onClick)
import Time


steps =
    25


colors =
    let
        hue =
            degrees 120

        sat =
            0.5
    in
        List.append
            (List.map (\li -> hsl hue sat li) (CV.spread 0.4 0.6 (steps / 2)))
            (List.map (\li -> hsl hue sat li) (CV.spread 0.6 0.4 (steps / 2)))


type alias Model =
    List Color.Color


rotate : List a -> List a
rotate list =
    case list of
        x :: xs ->
            List.append xs [ x ]

        [] ->
            []


type Msg
    = Tick


update : Msg -> List a -> ( List a, Cmd b )
update msg model =
    case msg of
        Tick ->
            ( rotate model, Cmd.none )


item x col =
    rectangle 20 80 |> solidFill col |> move x 0


squares colorSet =
    List.map2 item (CV.spread -300 300 steps) colorSet
        |> group


backdrop =
    rectangle CV.width 120
        |> solidFill (Color.grayscale 0.33)


warning =
    "Do not look directly into the lights."


view model =
    case model of
        [] ->
            Html.text "No model!"

        x :: _ ->
            [ backdrop
            , squares model
            ]
                |> group
                |> svg CV.width 132
                |> CV.withSourceLink "C1106.elm" warning


main =
    Html.App.program
        { init = ( colors, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every (1000 / 60) (always Tick)
        }
