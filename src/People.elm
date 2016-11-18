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
            , "Rink"
            , "Martha"
            , "Anne"
            , "Emmet"
            , "Bruce"
            , "Raymond"
            ]
