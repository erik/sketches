# recipebin

A teeny tiny personal recipe collection.

## routes sketch

rough notes on functionality.

| resource  | method   | path              | needs auth? | description                            |
| :-------  | :-----   | :---              | :---------- | :----------                            |
| `User`    | `POST`   | `/user`           | no          | Create a new user.                     |
| `User`    | `PUT`    | `/user`           | yes         | Update the current user.               |
| `Session` | `POST`   | `/session/login`  | yes         | Create a new login session.            |
| `Session` | `GET`    | `/session/logout` | yes         | End the current session.               |
| `Recipe`  | `GET`    | `/recipe`         | no          | Fetch list of recipes.                 |
| `Recipe`  | `POST`   | `/recipe`         | no          | Create a new recipe.                   |
| `Recipe`  | `GET`    | `/recipe/:id`     | no          | Show recipe.                           |
| `Recipe`  | `PUT`    | `/recipe/:id`     | yes         | Update recipe.                         |
| `Recipe`  | `DELETE` | `/recipe/:id`     | yes         | Delete recipe.                         |
| `Tag`     | `GET`    | `/tag`            | no          | Fetch list of tags.                    |
| `Tag`     | `GET`    | `/tag/:tag`       | no          | Fetch list of recipes by tag           |
| n/a       | `GET`    | `/`               | no          | Search for recipes by various filters. |
