"""Extract & normalize recipe content."""

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

import extruct


@dataclass
class Recipe:
    # Required fields
    name: str
    author: str
    image: str
    description: str

    # TODO: At some point ingredients + instructions will be getting
    # more complex to handle grouping etc.
    ingredients: List[str] = field(default_factory=list)
    instructions: List[str] = field(default_factory=list)

    # TODO: support optional fields.
    # yield: Optional[str]
    # category: Optional[str]
    # cook_time: Optional[str]
    # prep_time: Optional[str]
    # total_time: Optional[str]

    def is_valid(self) -> bool:
        required = ["name", "author", "image", "description"]
        return all(
            [
                not any(getattr(self, k, "") == "" for k in required),
                not any(
                    getattr(self, k, []) == []
                    for k in ["ingredients", "instructions"]
                ),
            ]
        )


def _coerce_scalar(v: Any) -> Any:
    if isinstance(v, list):
        return v[0] if v != [] else ""
    else:
        # TODO: more nuance. dicts?
        return str(v)


def _coerce_str(s: Any) -> str:
    if isinstance(s, str):
        return s
    elif isinstance(s, list):
        return str(_coerce_scalar(s))
    else:
        return str(s)


def _coerce_list(x: Optional[Any]) -> List[Any]:
    if x is None:
        return []
    return [x] if not isinstance(x, list) else x


def _sanitize_str(s: str) -> str:
    # TODO: this
    return s.strip()


@dataclass
class Microdata:
    _inner: Dict[str, Any]

    def get(self, k: str, default: Any = None) -> Any:
        return self._inner.get(k, default)

    def is_recipe_type(self) -> bool:
        return self.get("@type") == "Recipe"

    def extract_recipe(self) -> Recipe:
        """Given something approximating a JSON-LD object in roughly the
        shape of schema.org/Recipe, turn it into the type of recipe we
        care about.

        The returned object may or may not be valid.
        """

        def _(v: Any) -> Any:
            if isinstance(v, list):
                # TODO: breaks if v is not list of str
                return [s for s in map(_sanitize_str, v) if s != ""]
            elif isinstance(v, str):
                return _sanitize_str(v)
            return v

        return Recipe(
            name=_(self._get_str("name")),
            author=_(self._get_author()),
            image=_(self._get_str("image")),
            description=_(self._get_str("description")),
            ingredients=_(self._get_ingredients()),
            instructions=_(self._get_instructions()),
        )

    def _get_first(self, *ks: str, default: Any = None) -> Optional[Any]:
        """Return value for first key that is present in inner."""
        for k in ks:
            if k in self._inner:
                return self.get(k)
        return default

    def _get_str(self, k: str, default: str = "") -> str:
        s = _coerce_str(self.get(k, default))
        return s.strip()

    def _get_list(self, k: str) -> List[str]:
        return _coerce_list(self.get(k))

    def _get_author(self) -> Optional[str]:
        obj = self.get("author", {})
        if isinstance(obj, dict):
            return obj.get("name") if obj.get("@type") == "Person" else None
        return _coerce_str(obj)

    def _get_ingredients(self) -> List[str]:
        i = self._get_first(
            "recipeIngredient", "ingredients", "ingredient", default=[]
        )
        return _coerce_list(i)

    def _get_instructions(self) -> List[str]:
        i = self._get_first(
            "recipeInstruction",
            "recipeInstructions",
            "instructions",
            default=[],
        )
        instructions = []
        for inst in _coerce_list(i):
            if isinstance(inst, str):
                instructions.append(inst)
            elif isinstance(inst, dict) and inst.get("@type") == "HowToStep":
                if "text" in inst:
                    instructions.append(inst["text"])

        return instructions


def _extract_recipe_microdata(html: str) -> List[Microdata]:
    extracted = extruct.extract(
        html, syntaxes=["microdata", "json-ld"], uniform=True
    )

    data: List[Dict[str, Any]] = []
    for ex in extracted.values():
        if not ex or not isinstance(ex, list):
            continue

        # Try to normalize for some common variants of JSON-LD
        for v in ex:
            if "@graph" in v:
                data.extend(v["@graph"])
            elif v.get("@type") == "WebPage" and "mainEntity" in v:
                data.append(v["mainEntity"])
            else:
                data.append(v)

    return [m for m in map(Microdata, data) if m.is_recipe_type()]


def extract_recipes(html: str) -> List[Recipe]:
    """Given a string of HTML, try to parse and normalize the recipe
    microdata.
    """
    recipes = map(
        lambda r: r.extract_recipe(), _extract_recipe_microdata(html)
    )
    return [r for r in recipes if r.is_valid()]
