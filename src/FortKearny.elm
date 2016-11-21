module Main exposing (..)

import Css exposing (..)
import Health as H
import Html
import Html.App
import Html.Attributes as HA
import People as P
import Random
import Time


type alias Model =
    List (Maybe Person)


type alias Person =
    { name : String
    , age : Float
    , health : H.Condition
    , vitality : Float
    }


hospitalSize =
    12


m0 =
    (Just (Person "Luigi" 29 H.Healthy 1.0))
        :: List.repeat (hospitalSize - 1) Nothing


asHsl : Float -> String
asHsl x =
    "hsl(" ++ (toString (round (x * 120.0))) ++ ",40%,80%)"


styles =
    Css.asPairs >> HA.style


bedStyle health =
    let
        base =
            [ flexGrow (int 1)
            , flexBasis (pct 30)
            , border3 (px 3) solid (hex "888")
            , height (px 120)
            , marginTop (px 4)
            ]
    in
        case health of
            Nothing ->
                [ styles base ]

            Just (H.Dead) ->
                [ styles <| [ backgroundColor (hex "222"), color (hsl 0 0 1) ] ++ base ]

            Just (H.Healthy) ->
                [ styles <| [ backgroundColor (hex "EEE") ] ++ base ]

            Just (H.Injury _) ->
                [ styles <| [ backgroundColor (hsl 30 0.8 0.6) ] ++ base ]

            _ ->
                [ styles <| [ backgroundColor (hsl 60 0.8 0.6) ] ++ base ]


viewPerson patient =
    case patient of
        Nothing ->
            Html.div (bedStyle Nothing) [ Html.text "empty bed" ]

        Just patient ->
            Html.div (bedStyle (Just patient.health))
                [ Html.div [ styles [ fontWeight (int 800) ] ]
                    [ Html.text (patient.name ++ " " ++ (toString (round patient.age))) ]
                , Html.div [] [ Html.text ("Symptoms: " ++ (H.describeHealth patient)) ]
                , Html.div [] [ Html.text ("Diagnosis: " ++ (H.diagnose patient)) ]
                  --, Html.div [] [ Html.text (toString patient) ]
                ]


sourceLink =
    let
        href =
            "https://github.com/rsbohn/codevember2016/blob/master/src/FortKearny.elm"
    in
        Html.a [ HA.href href ] [ Html.text "(source)" ]


view model =
    let
        myflexbox =
            styles [ displayFlex, flexDirection row, flexWrap wrap ]
    in
        Html.div []
            [ Html.h1 [] [ Html.text "Fort Kearny Hospital 1849" ]
            , Html.div [ myflexbox ] <|
                List.map viewPerson model
            , Html.div [] [ sourceLink ]
            ]


type Msg
    = Tick
    | NewPerson Person
    | CheckHealth (List Dots)


mysubs _ =
    Sub.batch
        [ Time.every (1000 * 5) (always Tick)
        ]


assess : Maybe Person -> Maybe Person
assess person =
    case person of
        Nothing ->
            Nothing

        Just p ->
            case p.health of
                H.Dead ->
                    Nothing

                H.Healthy ->
                    Nothing

                _ ->
                    person


isNothing a =
    case a of
        Nothing ->
            True

        _ ->
            False


hasEmptyBed model =
    case List.filter isNothing model of
        [] ->
            False

        _ ->
            True


takeNextBed : Person -> Model -> Model
takeNextBed person model =
    let
        fillBed ss m =
            case m of
                [] ->
                    model

                p :: ps ->
                    if isNothing p then
                        List.append ss (Just (person) :: ps)
                    else
                        fillBed (List.append ss [ p ]) ps
    in
        fillBed [] model


heal : Maybe Person -> Dots -> Maybe Person
heal person ( better, choise ) =
    case person of
        Nothing ->
            Nothing

        Just p ->
            if better < 0.01 then
                Just ({ p | health = H.Dead })
            else if better > 0.9 then
                Just ({ p | health = H.Healthy })
            else
                person


update msg model =
    case msg of
        Tick ->
            let
                cleaned =
                    List.map assess model
            in
                if hasEmptyBed cleaned then
                    ( cleaned
                    , Cmd.batch
                        [ Random.generate NewPerson randomPerson
                        , Random.generate CheckHealth randomProgress
                        ]
                    )
                else
                    ( cleaned, Random.generate CheckHealth randomProgress )

        NewPerson person ->
            case person.health of
                --Dead -> (model, Cmd.none)
                _ ->
                    ( takeNextBed person model, Cmd.none )

        CheckHealth package ->
            -- apply the package to the model
            ( List.map2 heal model package, Cmd.none )


randomPerson : Random.Generator Person
randomPerson =
    let
        it k name malady =
            { name = name, age = (k * 20 + 5), health = malady, vitality = 0.78 }
    in
        Random.map3 it (Random.float 0 1) P.nextName H.randomMalady


type alias Dots =
    ( Float, Float )


randomProgress : Random.Generator (List (Dots))
randomProgress =
    Random.list hospitalSize (Random.pair (Random.float 0 1) (Random.float 0 1))


main =
    Html.App.program
        { init = ( m0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = mysubs
        }
