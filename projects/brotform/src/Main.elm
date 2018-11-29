port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view =
            \model ->
                { title = "brotform"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Weight =
    Int


type alias Ingredient =
    { isFlour : Bool
    , name : String
    , weight : Weight
    }


type alias Formula =
    List Ingredient


type alias Model =
    { formula : Formula }


initModel : Model
initModel =
    { formula =
        [ { name = "Bread flour", isFlour = True, weight = 700 }
        , { name = "Semolina flour", isFlour = True, weight = 300 }
        , { name = "Water", isFlour = False, weight = 500 }
        , { name = "Salt", isFlour = False, weight = 10 }
        , { name = "Yeast", isFlour = False, weight = 1 }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


totalFlourWeight : Formula -> Weight
totalFlourWeight formula =
    formula
        |> List.filterMap
            (\i ->
                if i.isFlour then
                    Just i.weight
                else
                    Nothing
            )
        |> List.sum


totalIngredientWeight formula =
    formula |> List.map (\i -> i.weight) |> List.sum


bakersPercentage : Ingredient -> Weight -> Int
bakersPercentage ingredient totalFlour =
    let
        ratio =
            toFloat ingredient.weight / toFloat totalFlour

        pct =
            ratio * 100
    in
        round pct



-- UPDATE


type Msg
    = NoOp
    | AddIngredient


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddIngredient ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "formula" ]
        , viewFormula model.formula
        ]


viewFormula formula =
    let
        flourWeight =
            totalFlourWeight formula

        totalIngredient =
            { isFlour = False
            , weight = totalIngredientWeight formula
            , name = "Total"
            }

        ingredients =
            formula
                |> List.map (viewIngredient flourWeight)
    in
        div [ class "formula" ]
            [ div [ class "ingredient-list" ] ingredients
            , viewIngredient flourWeight totalIngredient
            ]


viewIngredient : Weight -> Ingredient -> Html a
viewIngredient flourWeight ingredient =
    div []
        [ span [] [ text <| (String.fromInt ingredient.weight) ++ "g" ]
        , span [] [ text <| ingredient.name ]
        , span [] [ text <| (String.fromInt <| bakersPercentage ingredient flourWeight) ++ "%" ]
        ]
