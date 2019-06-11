module Route exposing (Route(..), UrlSecret, fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, map, oneOf, s)



-- ROUTING


type Route
    = NotFound
    | CreateSecret
    | ViewSecret UrlSecret


type alias UrlSecret =
    { id : String
    , key : Maybe String
    }


{-| Return the corresponding `Route` for a given URL, falling back to
`NotFound`.
-}
fromUrl : Url -> Route
fromUrl url =
    url
        |> Parser.parse urlParser
        |> Maybe.withDefault NotFound


urlParser : Parser (Route -> a) a
urlParser =
    oneOf
        [ map CreateSecret Parser.top
        , map ViewSecret (s "secret" </> parseUrlSecret)
        ]


parseUrlSecret : Parser (UrlSecret -> a) a
parseUrlSecret =
    let
        parser =
            Parser.string </> Parser.fragment identity

        -- TODO: How to handle missing fragment?
        toUrlSecret id frag =
            { id = id, key = frag }
    in
    map toUrlSecret parser
