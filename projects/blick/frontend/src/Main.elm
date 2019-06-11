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
import Route as Route exposing (Route(..))
import Task
import Url exposing (Url)


type Model
    = NotFound
    | CreateSecret
    | ViewSecret


type Msg
    = NoOp
    | FetchedSecret (Result Http.Error Secret)


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


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    routeUrl initialModel url


fetchSecret : Route.UrlSecret -> Cmd Msg
fetchSecret secret =
    let
        id =
            secret.id

        -- Associate the secret key we extracted from the URL to the
        -- response payload the server sent us.
        attachSecretKey result =
            result
                |> Result.map (\r -> { r | key = secret.key })
                |> FetchedSecret
    in
    -- TODO: Remove localhost
    Http.get
        { url = "http://localhost:8080/secret/" ++ id
        , expect = Http.expectJson attachSecretKey secretDecoder
        }


initialModel : Model
initialModel =
    CreateSecret



-- ROUTING


routeUrl : Model -> Url -> ( Model, Cmd Msg )
routeUrl model url =
    let
        route =
            Route.fromUrl url
    in
    chooseRoute route model


chooseRoute : Route -> Model -> ( Model, Cmd Msg )
chooseRoute route model =
    case route of
        Route.NotFound ->
            ( NotFound, Cmd.none )

        Route.CreateSecret ->
            ( CreateSecret, Cmd.none )

        Route.ViewSecret id ->
            ( ViewSecret, fetchSecret id )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "blick"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div []
        [ h1 [] [ text <| Debug.toString model ]
        , text "that's your model"
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- TODO: Implement this one
        FetchedSecret result ->
            case result of
                Ok secret ->
                    Debug.log (Debug.toString secret)
                        |> (\_ -> ( model, Cmd.none ))

                Err err ->
                    Debug.log ("Failed to receive secret" ++ Debug.toString err)
                        |> (\_ -> ( model, Cmd.none ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- SECRETS


type alias Secret =
    { blob : String
    , creationDate : String
    , expirationDate : Maybe String
    , key : Maybe String
    }


secretDecoder : Json.Decoder Secret
secretDecoder =
    Json.map4 Secret
        (Json.field "blob" Json.string)
        -- TODO: Need to do the next step of converting these to date objects.
        (Json.field "creationDate" Json.string)
        (Json.field "expirationDate" (Json.nullable Json.string))
        -- Key field comes from the URL, not the server
        (Json.succeed <| Nothing)
