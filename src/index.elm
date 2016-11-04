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
        [ a [ href (base ++ "/src/" ++ project.source) ]
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
    ]
