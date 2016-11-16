module T1115 exposing (..)

import Html
import Html.App
import Html.Attributes as HA
import M1107 as M
import Markdown as MD
import Random
import Random.Extra
import Time


slug =
    "T1114 Fort Kearny Medic\n\nThis week I'm building a medical simulator for 1849."


type alias Person =
    { name : String
    , age : Float
    , health : Condition
    }


type Condition
    = Healthy
    | Illness { name : String, symptoms : List String, duration : Duration }
    | Injury { name : String, description : String }
    | Dead


type Duration
    = Ongoing
    | Days Int
    | Hours Int


type alias Model =
    List Person


sketch0 =
    [ person0 ]


person0 =
    Person "Harry" 36.0 Healthy


infection =
    Illness
        { name = "Infection"
        , symptoms = [ "fever", "swelling", "redness", "pus" ]
        , duration = Ongoing
        }


gangrene =
    Illness
        { name = "Gangrene"
        , symptoms = [ "fever", "swelling", "putrefication", "green pus", "foul odor" ]
        , duration = Ongoing
        }


shoot p =
    { p
        | health = Injury { name = "Gunshot Wound", description = "bullet hole, bleeding" }
    }


sicken p =
    { p
        | health =
            Illness
                { name = "Grippe"
                , symptoms = [ "nasal congestion", "fever", "cough" ]
                , duration = (Days 5)
                }
    }


nextName : Random.Generator String
nextName =
    Random.Extra.choices <|
        List.map Random.Extra.constant
            [ "Simon"
            , "Pedro"
            , "Eliza"
            , "Rink"
            , "Martha"
            , "Anne"
            , "Emmet"
            , "Bruce"
            , "Raymond"
            ]


randomMalady : Random.Generator Condition
randomMalady =
    Random.Extra.choices <|
        List.map Random.Extra.constant
            [ Illness { name = "Influenza", duration = (Days 8), symptoms = [ "fever", "body ache", "congestion" ] }
            , Illness { name = "Cholera", duration = (Days 4), symptoms = [ "severe diarrhea", "vomiting", "lethargy", "abdominal pain" ] }
            , Injury { name = "gunshot wound", description = "bullet hole, bleeding" }
            , Injury { name = "mauled by a bear", description = "claw marks, lacerations, bleeding" }
            , Injury { name = "broken arm", description = "redness, swelling, unnatural position of arm" }
            , Dead
            ]


randomPerson : Random.Generator Person
randomPerson =
    let
        it k name malady =
            { name = name, age = (k * 20 + 5), health = malady }
    in
        Random.map3 it (Random.float 0 1) nextName randomMalady


update msg model =
    case msg of
        Tick ->
            ( model, Random.generate NewPerson randomPerson )

        NewPerson p ->
            let
                persons =
                    List.take 6 <| p :: model
            in
                ( persons, Cmd.none )


describeHealth person =
    case person.health of
        Illness i ->
            toString i.symptoms

        Injury i ->
            i.description

        _ ->
            "No obvious problems."


diagnose person =
    case person.health of
        Healthy ->
            "Clean bill of health."

        Illness i ->
            i.name

        Injury i ->
            i.name

        Dead ->
            "Death"


indent =
    HA.style [ ( "marginLeft", "2em" ) ]


view person =
    Html.div []
        [ Html.h2 [] [ Html.text person.name ]
        , Html.div [ indent ] [ Html.text <| "Symptoms: " ++ (describeHealth person) ]
        , Html.div [ indent ] [ Html.text <| "Diagnosis: " ++ (diagnose person) ]
        ]


viewWithMarkup : List Person -> Html.Html a
viewWithMarkup model =
    Html.div []
        [ Html.div [ HA.class "patients" ]
            (List.map view model)
        , Html.div []
            [ M.sourceLink "M1112.elm"
            ]
        , MD.toHtml [] slug
        ]


type Msg
    = Tick
    | NewPerson Person


mysubs model =
    Sub.batch
        [ Time.every (10 * 1000) (always Tick) ]


main =
    Html.App.program
        { init = ( sketch0, Cmd.none )
        , update = update
        , view = viewWithMarkup
        , subscriptions = mysubs
        }
