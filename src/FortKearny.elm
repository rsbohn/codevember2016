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


bedStyle health =
    let
        base =
            [ ( "flexGrow", "1" )
            , ( "flexBasis", "30%" )
            , ( "border", "solid 3px #888" )
            , ( "height", "120px" )
            , ( "marginTop", "4px" )
            ]

        vit =
            0.2
    in
        case health of
            Nothing ->
                [ HA.style base ]

            Just (H.Dead) ->
                [ HA.style <| ( "background", "#222" ) :: ( "color", "white" ) :: base ]

            Just (H.Healthy) ->
                [ HA.style <| ( "background", "silver" ) :: base ]

            Just (H.Injury _) ->
                [ HA.style <| ( "background", "hsl(30,80%,60%)" ) :: base ]

            _ ->
                [ HA.style <| ( "background", "hsl(60,80%,60%)" ) :: base ]


viewPerson patient =
    case patient of
        Nothing ->
            Html.div (bedStyle Nothing) [ Html.text "empty bed" ]

        Just patient ->
            Html.div (bedStyle (Just patient.health))
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
        [ Time.every (1000 * 5) (always Tick)
        ]


checkout : Maybe Person -> Maybe Person
checkout person =
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


update msg model =
    case msg of
        Tick ->
            let
                cleaned =
                    List.map checkout model
            in
                if hasEmptyBed cleaned then
                    ( cleaned, Random.generate NewPerson randomPerson )
                else
                    ( cleaned, Cmd.none )

        NewPerson person ->
            case person.health of
                --Dead -> (model, Cmd.none)
                _ ->
                    ( takeNextBed person model, Cmd.none )

        CheckHealth ->
            ( model, Cmd.none )


randomPerson : Random.Generator Person
randomPerson =
    let
        it k name malady =
            { name = name, age = (k * 20 + 5), health = malady, vitality = 0.78 }
    in
        Random.map3 it (Random.float 0 1) P.nextName H.randomMalady


main =
    Html.App.program
        { init = ( m0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = mysubs
        }
