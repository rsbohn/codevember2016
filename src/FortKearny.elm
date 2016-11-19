module Main exposing (..)

import People as P
import Health as H
import Html
import Html.App
import Html.Attributes as HA
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
    (Just (Person "Luigi" 29 H.Healthy))
        :: List.repeat 5 Nothing


bedStyle =
    [ HA.style
        [ ( "border", "solid 3px #888" )
        , ( "marginTop", "4px" )
        , ( "flexGrow", "1" )
        , ( "flexBasis", "30%" )
        , ( "height", "120px" )
        ]
    ]


viewPerson patient =
    case patient of
        Nothing ->
            Html.div bedStyle [ Html.text "empty bed" ]

        Just patient ->
            Html.div bedStyle
                [ Html.div [ HA.style [ ( "fontWeight", "800" ) ] ]
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
    Html.div []
        [ Html.h1 [] [ Html.text "Fort Kearny Hospital 1849" ]
        , Html.div [ HA.style [ ( "display", "flex" ), ( "flexFlow", "row wrap" ), ( "flexWrap", "ltr" ) ] ] <|
            List.map viewPerson model
        , Html.div [] [ sourceLink ]
        ]


type Msg
    = Tick
    | NewPerson Person
    | CheckHealth


mysubs _ =
    Sub.batch
        [ Time.every (1000 * 10) (always Tick)
        ]


undertaker : Maybe Person -> Maybe Person
undertaker person =
    case person of
        Nothing ->
            Nothing

        Just p ->
            case p.health of
                H.Dead ->
                    Nothing

                _ ->
                    person


update msg model =
    case msg of
        Tick ->
            ( model, Random.generate NewPerson randomPerson )

        NewPerson p ->
            let
                m =
                    List.take 6 (Just p :: model)
                        |> List.map undertaker
            in
                ( m, Cmd.none )

        CheckHealth ->
            ( model, Cmd.none )


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
