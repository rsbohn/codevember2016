module M1107 exposing (..)

-- Machine module 1107 -- first module

import Color exposing (Color, hsl, hsla)
import Color.Convert exposing (hexToColor)
import Graphics.Render exposing (..)
import Html exposing (Html)
import Html.App
import Html.Attributes as HA
import Http
import Markdown as MD
import Task
import Time


sketch =
    { width = 720
    , height = 500
    , srcBaseUrl =
        "https://github.com/rsbohn/codevember2016"
    }


type alias Model =
    { colors : List (Color)
    , frame : Float
    }


mview : Model -> Html msg
mview model =
    let
        fill =
            case List.head model.colors of
                Nothing ->
                    hsla (degrees 60) 0.5 0.5 0.1

                Just c ->
                    c

        backdrop =
            rectangle sketch.width sketch.height
                |> solidFill (hsl (degrees 25) 0.05 0.75)
    in
        group
            [ backdrop
            , spacer |> move (-1 * sketch.width * 0.48) 0
            , spacer |> move (1 * sketch.width * 0.48) 0
            , List.indexedMap
                (\n color ->
                    rect 20 color
                        |> spiralShape n
                        |> rotate (model.frame / 20)
                )
                model.colors
                |> group
                |> move 80 -80
            , monitor (toString model.frame)
                |> move (-1 * sketch.width * 0.46) 0
            ]
            |> svg sketch.width sketch.height


monitor =
    plain 24 "Source Sans Pro" Color.black


rect size color =
    rectangle size size |> solidFill color


spiralShape n =
    let
        nf =
            toFloat n

        r =
            nf * nf * 3

        dx =
            r * (cos (nf * 15 * pi / 180))

        dy =
            r * (sin (nf * 15 * pi / 180))
    in
        move dx dy


spacer =
    rectangle 12 500 |> solidFill (hsl (degrees 12) 0.3 0.3)


sourceLink src =
    Html.a
        [ HA.href (sketch.srcBaseUrl ++ "/blob/master/src/" ++ src)
        , HA.style [ ( "width", "12em" ) ]
        ]
        [ Html.text "(source)" ]


body : Model -> Html msg -> Html msg -> Html msg -> Html msg
body model srcLink title markdown =
    Html.div []
        [ mview model
        , Html.div []
            [ Html.span [] [ srcLink ]
            , Html.span [ HA.style [ ( "marginLeft", "20%" ) ] ]
                [ title ]
            ]
        , Html.div [] [ markdown ]
        ]


view : Model -> Html.Html msg
view model =
    body model
        (sourceLink "M1107.elm")
        (Html.text "Module 1107")
        (MD.toHtml [ HA.class "markdown" ] """
Shows the last five minutes of cheer lights.

[http://cheerlights.com]

        """)


type Msg
    = Tick
    | UpdateCheer
    | NextColor String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | frame = model.frame + 1 }, Cmd.none )

        UpdateCheer ->
            ( model, fetchCheer )

        NextColor s ->
            case (hexToColor s) of
                Nothing ->
                    ( { model | colors = [] }, Cmd.none )

                Just aColor ->
                    ( { model | colors = List.take 12 (aColor :: model.colors) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1000 / 4) (always Tick)
        , Time.every (1000 * 25) (always UpdateCheer)
        ]


cheerlights =
    "//api.thingspeak.com/channels/1417/field/2/last.txt"


fetchColor =
    always <| NextColor "xxx"


fetchCheer : Cmd Msg
fetchCheer =
    Http.getString cheerlights
        |> Task.perform (always <| NextColor "") NextColor


translucent : Color.Color -> Color.Color
translucent c =
    let
        original =
            Color.toHsl c
    in
        hsla original.hue original.saturation original.lightness 0.3


model0 =
    { colors = List.repeat 12 (translucent Color.charcoal), frame = 0 }


main =
    Html.App.program
        { init = ( model0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
