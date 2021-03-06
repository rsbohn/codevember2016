module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Table


main =
    App.program
        { init = init projects
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { projects : List Project
    , tableState : Table.State
    , query : String
    }


init : List Project -> ( Model, Cmd Msg )
init projects =
    let
        model =
            { projects = projects
            , tableState = Table.initialSort "Day"
            , query = ""
            }
    in
        ( model, Cmd.none )


type Msg
    = SetQuery String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }, Cmd.none )


view : Model -> Html Msg
view { projects, tableState, query } =
    let
        lowerQuery =
            String.toLower query

        filtered =
            List.filter (String.contains lowerQuery << String.toLower << .title) projects
    in
        div []
            [ h1 [] [ text "Codevember 2016 by rsbohn" ]
            , div [ style [ ( "marginLeft", "20px" ) ] ]
                [ input [ placeholder "search by title", onInput SetQuery ] []
                , Table.view config tableState filtered
                ]
            , div [] [ text "..." ]
            ]


config : Table.Config Project Msg
config =
    Table.config
        { toId = .day
        , toMsg = SetTableState
        , columns =
            [ dayColumn
            , Table.stringColumn "Title" .title
            , sourceColumn
            ]
        }


linkToProject : Project -> Html Msg
linkToProject project =
    a [ href (project.day ++ ".html") ] [ text project.day ]


dayColumn : Table.Column Project Msg
dayColumn =
    Table.veryCustomColumn
        { name = "Day"
        , viewData = \data -> { attributes = [], children = [ linkToProject data ] }
        , sorter = Table.increasingOrDecreasingBy .day
        }


base =
    "https://github.com/rsbohn/codevember2016"


linkToSource : Project -> { attributes : List a, children : List (Html b) }
linkToSource project =
    { attributes = []
    , children =
        [ a [ href (base ++ "/blob/master/src/" ++ project.source) ]
            [ text project.source ]
        ]
    }


sourceColumn : Table.Column Project Msg
sourceColumn =
    Table.veryCustomColumn
        { name = "Source Link"
        , viewData = linkToSource
        , sorter = Table.unsortable
        }


type alias Project =
    { day : String
    , title : String
    , source : String
    }


projects : List Project
projects =
    [ Project "C1102" "Color Wheel" "C1102.elm"
    , Project "C1103" "Battletowers" "C1103.elm"
    , Project "C1104" "Iberian Rose" "C1104.elm"
    , Project "C1105" "Carpeted Garage" "C1105.elm"
    , Project "C1106" "Do not look into the light" "C1106.elm"
    , Project "M1107" "Cheer Lights" "M1107.elm"
    , Project "C1108" "I voted!" "C1108.elm"
    , Project "M1109" "Click and Rotate" "M1109.elm"
    , Project "M1110" "Blinkenlights" "M1110.elm"
    , Project "M1112" "Pencil Rain" "M1112.elm"
    , Project "T1115" "Fort Kearny Medic pt I" "M1115.elm"
    , Project "C1122" "Birthday Wishes" "C1122.elm"
    , Project "C1123" "The Worm" "C1123.elm"
    , Project "C1124" "Double Infinity" "C1124.elm"
    ]
