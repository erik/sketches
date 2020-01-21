# RecipeBin

RecipeBin is a simple web service to manage a personal recipe
collection. Recipes can be added on the site via a simple form, or
automatically scraped from arbitrary sources on the web, using an
importer.

## Goals

### Initial

- API to trigger detection of recipes from a named site. Ideally, this
  is eventually runnable inside of a bookmarklet, but don't need to
  aim for this immediately (since I don't use bookmarklets)
    - JSON+LD and HTML microdata for the `schema.org/Recipe` type.
    - No support for dynamically generated HTML content (React,
      etc.). All data must be visible via a single `curl` command.
    - Heavily lean on existing projects
      (e.g. [extruct](https://github.com/scrapinghub/extruct)) to
      perform the actual parsing + detection
    - Minimal normalization to coerce recipes into a sane format. It's
      fine if users have to clean up a bit manually as long as we get
      80% there.

- User + authentication support only for API requests. Will use a
  token scheme for this.
    - Token will be opaque and generated on the server side.

- Read-only web view
    - This will be throwaway! Do not invest time here.
    - No authentication. Everything is world visible.
    - Functionality: list all recipes, show single recipe

- Command line admin tool
    - Create new user


### Future

#### Near

- Edit existing recipes
  - Throwaway form UI, simple data validation
- API to accept recipe in already nicely cleaned up JSON format,
  possibly for use with other tools, e.g. like...
- Integration with RecipeCleaner. Should be able to log in via the
  extension and then use the API to save recipes.

#### Futher

- Grocery view: select multiple recipes, combine ingredient counts,
  display a TODO list of which ingredients to buy.
- Decent UI. Rework all HTML content to be mobile-friendly and not
  awful to look at.
      - Current thought is minimal JS use for enhancements.

#### Far

- User management views. Web signup form, allow random internet people
  to sign up.
- Resource limits. x API requests / hr, x recipes stored.
- If there is interest: support a paid tier for unlimited recipes,
  potentially more features.

## Details

### Architecture

First pass will run entirely within a single process. Eventually,
there will likely be a queue and an offline worker process used to
scrape recipe site data, but since the traffic volume initially will
be zero, there isn't really a benefit initially.

Proposed stack:

- python3.7
- postgres
- sqlalchemy
- flask
- extruct

### Schema

Recipe data will be stored as a JSON blob inside Postgres. Postgres is
boring, and can expand to support any future additions to this basic
CRUD app.

The `jsonb` type can also be indexed, so lookups e.g. by ingredient
can be efficient. Additionally, there's good support for full-text
search.

#### SQL Schema

Basic tables for initial pass.

```sql
CREATE TABLE users(
    id       INTEGER          PRIMARY KEY
  , email    TEXT    NOT NULL
  , pw_hash  TEXT    NOT NULL

  , created_at TIMESTAMP NOT NULL
  , updated_at TIMESTAMP
  , deleted_at TIMESTAMP

  , CONSTRAINT users_lower_email_unique UNIQUE (lower(email))
);

CREATE TABLE recipes(
    id         INTEGER            PRIMARY KEY
  , user_id    INTEGER   NOT NULL REFERENCES users(id)
  , recipe     JSONB     NOT NULL

  , created_at TIMESTAMP NOT NULL
  , updated_at TIMESTAMP
  , deleted_at TIMESTAMP
);
CREATE INDEX recipes_full_text ON recipes USING gin (to_tsvector('english', recipe))

CREATE TABLE user_api_tokens(
    token      TEXT               PRIMARY KEY
  , user_id    INTEGER   NOT NULL REFERENCES users(id)

  , created_at TIMESTAMP NOT NULL
  , deleted_at TIMESTAMP
);
CREATE INDEX user_api_tokens_by_user ON user_api_tokens USING (user_id);
```

#### JSON Schema

The normalized JSON stored in the DB will still be valid
`schema.org/Recipe` content, but stripped of the things that we don't
care about, and without the wide variation of possibilities.

Further reading:

- http://schema.org/Recipe
- https://developers.google.com/search/docs/data-types/recipe#recipe-properties

```json
{
  "@context": "http://schema.org",
  "@type": "Recipe",

  // Required
  "author": String,
  "image": URL,
  "description": String,
  "recipeIngredient": List<String>, // TODO: How do we support grouping ingredients, or parsing out quantity?
  "recipeInstructions": List<HowToSection|Text>,

  // Optional
  "recipeYield": "6",
  "recipeCategory": String?, // "dinner", "dessert", "snack", ...
  "cookTime": String,  // ISO8601 duration e.g. "PT1H"
  "prepTime": String,  // ...
  "totalTime": String, // ...
}
```

### API

Initial API surface will only need to be the single endpoint to
trigger the scraper.

```
POST /api/v1/recipes/scrape

Authentication: Bearer $TOKEN
Content-Type: application/json

{ "url": "http://recipes.example.org" }


{ "id": "123" }
```
