port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Json
import Task
import Url exposing (Url)


type Model
    = NotFound
    | CreateSecret
    | ViewSecret


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions

        -- TODO: need real types for this
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }



-- INIT


-- TODO: Route based on initial URL.
init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    CreateSecret



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "blick"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [] [ text "hello world" ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
