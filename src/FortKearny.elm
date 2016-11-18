module Main exposing (..)

import People as P
import Health as H
import Html
import Html.App
import Random
import Time


type alias Model =
    List (Maybe Person)


type alias Person =
    { name : String
    , age : Float
    , health : H.Condition
    }


m0 =
    [ Just <| Person "Luigi" 29 H.Healthy
    ]


view model =
    Html.text (toString model)


type Msg
    = Tick
    | NewPerson Person


mysubs _ =
    Sub.batch
        [ Time.every (1000 * 1) (always Tick)
        ]


update msg model =
    case msg of
        Tick ->
            ( model, Random.generate NewPerson randomPerson )

        NewPerson p ->
            let
                m =
                    List.take 6 (Just p :: model)
            in
                ( m, Cmd.none )


randomPerson : Random.Generator Person
randomPerson =
    let
        it k name malady =
            { name = name, age = (k * 20 + 5), health = malady }
    in
        Random.map3 it (Random.float 0 1) P.nextName H.randomMalady


main =
    Html.App.program
        { init = ( m0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = mysubs
        }
