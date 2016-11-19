module People exposing (..)

import Random
import Random.Extra


nextName : Random.Generator String
nextName =
    Random.Extra.choices <|
        List.map Random.Extra.constant
            [ "Simon"
            , "Pedro"
            , "Eliza"
            , "Sarah"
            , "Martha"
            , "Anne"
            , "Emmet"
            , "Bruce"
            , "Raymond"
            , "Abraham"
            , "Jenny"
            , "Douglas"
            , "Louisa May"
            , "Brad"
            , "Nathaniel"
            , "George"
            , "Georgia"
            , "Ruth"
            , "Eleazar"
            , "Hannaniah"
            , "Bart"
            , "Shane"
            , "Ellen"
            ]
