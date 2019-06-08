port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task


type alias Model =
    {
    }

type Msg
    = NoOp

main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view model =
    { title = "blick"
    , body = [viewBody model]
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        NoOp ->
            ( model, Cmd.none )


viewBody : Model -> Html Msg
viewBody model = 
    div [] [ text "hello world" ]
