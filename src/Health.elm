module Health exposing (..)


type Status
    = Healthy
    | Sick Disease
    | Dead


type Disease
    = Infection
    | Gangrene
    | Typhus
    | Cholera
    | GSW
    | Dysentery
