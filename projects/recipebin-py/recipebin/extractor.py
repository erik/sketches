"""Extract & normalize recipe content."""

from typing import Any, Dict, List, Optional

import extruct


class Recipe:
    # Required fields
    title: str
    author: str
    image: str
    description: str
    recipe_ingredient: List[str]
    recipe_instructions: List[str]

    # TODO: support optional fields.
    # recipe_yield: Optional[str]
    # recipe_category: Optional[str]
    # cook_time: Optional[str]
    # prep_time: Optional[str]
    # total_time: Optional[str]


def _extract_microdata(s: str) -> Any:
    def _is_recipe_type(d: Dict[str, str]) -> bool:
        expected = [
            ("@context", "https://schema.org"),
            ("@id", "Recipe"),
        ]

        return all(d.get(k) == v for (k, v) in expected)

    data = extruct.extract(s, syntaxes=["microdata", "json-ld"], uniform=True)
    recipes = filter(_is_recipe_type, data)
    return recipes


def extract_recipe(s: str) -> Optional[Recipe]:

    pass
