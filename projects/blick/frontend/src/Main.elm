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
    | FetchSecret
    | ViewSecret Secret


type Msg
    = NoOp
    | FetchedSecret (Result Http.Error Secret)
    | SetSecretKey String


port showKeyPrompt : String -> Cmd msg


port showKeyPromptResult : (String -> msg) -> Sub msg


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
                |> Debug.log "Received secret"
                |> FetchedSecret
    in
    -- TODO: Remove localhost, make this somehow configurable
    Http.get
        { url = "http://localhost:8080/api/secret/" ++ id
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
            ( FetchSecret, fetchSecret id )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        NotFound ->
            { title = "not found"
            , body = [ viewNotFound ]
            }

        ViewSecret secret ->
            { title = "blick"
            , body = [ viewSecret secret ]
            }

        FetchSecret ->
            { title = "blick - fetching"
            , body = [ viewFetchSecret ]
            }

        CreateSecret ->
            { title = "blick"
            , body = [ viewNotImplemented model ]
            }



-- TODO: Need to style this


viewWrapper : List (Html Msg) -> Html Msg
viewWrapper body =
    div [ class "content" ] body


viewSecret : Secret -> Html Msg
viewSecret secret =
    viewWrapper
        [ div []
            [ h1 []
                [ text "Got the secret!" ]
            , text <| Debug.toString secret
            ]
        ]



-- TODO: Spinner? Something a little more progress-y?


viewFetchSecret : Html Msg
viewFetchSecret =
    viewWrapper
        [ p [] [ text "... fetching ..." ]
        ]


viewNotFound : Html Msg
viewNotFound =
    viewWrapper
        [ h1 [] [ text "That's a 404." ]
        , p [] [ text "Sorry, page not found." ]
        ]


viewNotImplemented : Model -> Html Msg
viewNotImplemented model =
    div []
        [ h1 [] [ text <| Debug.toString model ]
        , text "that's your model"
        , br [] []
        , text "it's not implemented yet though..."
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- TODO: Split this out, too much rightward drift.
        FetchedSecret result ->
            case result of
                Ok secret ->
                    let
                        -- If we have a key, nothing to do, otherwise show the prompt.
                        cmd =
                            secret.key
                                |> Maybe.map (\_ -> Cmd.none)
                                |> Maybe.withDefault (showKeyPrompt "enter secret password")
                    in
                    ( ViewSecret secret, cmd )

                -- TODO: Implement some kind of error handling
                Err err ->
                    Debug.log ("Failed to receive secret" ++ Debug.toString err)
                        |> (\_ -> ( model, Cmd.none ))

        -- Manually entered secret key.
        -- TODO: Probably should be renamed.
        SetSecretKey key ->
            case model of
                ViewSecret secret ->
                    let
                        k =
                            -- Don't allow empty string passwords
                            case key of
                                "" ->
                                    Nothing

                                x ->
                                    Just key
                    in
                    ( ViewSecret { secret | key = k }
                    , Cmd.none
                    )

                -- If we're not in ViewSecret, nothing to do here.
                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    showKeyPromptResult SetSecretKey



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
