module Health exposing (..)

import Random
import Random.Extra


type Condition
    = Healthy
    | Illness { name : String, symptoms : List String, duration : Duration }
    | Injury { name : String, description : String }
    | Dead


type Duration
    = Ongoing
    | Days Int
    | Hours Int


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


randomMalady : Random.Generator Condition
randomMalady =
    Random.Extra.choices <|
        List.map Random.Extra.constant
            [ Illness { name = "Influenza", duration = (Days 8), symptoms = [ "fever", "body ache", "congestion" ] }
            , Illness { name = "Cholera", duration = (Days 4), symptoms = [ "severe diarrhea", "vomiting", "lethargy", "abdominal pain" ] }
            , Injury { name = "gunshot wound", description = "bullet hole, bleeding" }
            , Injury { name = "mauled by a bear", description = "claw marks, lacerations, bleeding" }
            , Injury { name = "broken arm", description = "redness, swelling, unnatural position of arm" }
            , Healthy
            , Dead
            ]


describeHealth person =
    case person.health of
        Illness i ->
            toString i.symptoms

        Injury i ->
            i.description

        Dead ->
            "No pulse."

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
