import { h, app } from 'hyperapp'; // eslint-disable-line no-unused-vars
import browser from 'webextension-polyfill';

const actions = {};

// Callback for onclick events.
function toggleStrikethrough () {
    this.classList.toggle('strikethrough');
}

function view (recipe) {
    return (
        <div id="wrapper">
            { viewHeader(recipe) }

            <main>
                <div className="grid">
                    { viewIngredients(recipe) }
                    { viewInstructions(recipe) }
                </div>
            </main>

            <script type="application/ld+json">
                { JSON.stringify(recipe.original) }
            </script>
        </div>
    );
}

function viewHeader (recipe) {
    const image = recipe.image ? <img src={recipe.image} /> : null;
    const hostname = (new URL(recipe.url)).hostname;

    const bylineParts = [
        recipe.author && (<span> By { recipe.author } </span>),
        recipe.yield && (<span> Yields { recipe.yield } </span>),
        recipe.time && (<span> { recipe.time } </span>),
        <span>Via <a href={ recipe.url }>{ hostname }</a></span>
    ].filter(e => e);

    let byline = [];
    bylineParts.forEach((e, i) => {
        byline.push(e);
        if (i !== bylineParts.length - 1) {
            byline.push(<span> | </span>);
        }
    });

    return (
        <header>
            <h1>{ recipe.name }</h1>
            <div id="byline"> { byline }</div>

            <section id="meta">
                { image }
                <p id="description">
                    { recipe.description }
                </p>
            </section>
        </header>
    );
}

function viewIngredients (recipe) {
    let ingredients = recipe.ingredients.map(i => {
        let quantity = i.quantity ?
            <b className="quantity">{ i.quantity } { i.unit || '' }</b>
            : null;

        return (
            <li className="ingredient"
                onclick={toggleStrikethrough}>
                { quantity } { i.ingredient }
            </li>
        );
    });

    return (
        <section id="ingredients">
            <h2>Ingredients</h2>
            <ul> { ingredients } </ul>
        </section>
    );
}

function viewInstructions (recipe) {
    let instructionElem;

    if (recipe.instructionText) {
        instructionElem = <p> { recipe.instructionText } </p>;
    } else if (recipe.instructionList) {
        let instructions = recipe.instructionList.map(i => (
            <li className="instruction"> { i } </li>
        ));

        instructionElem = <ol> { instructions } </ol>;
    } else {
        instructionElem = (
            <div>
                <p> Sorry, seems this recipe did not include any instructions. </p>
                <p> The recipe you tried to view was not properly formatted. </p>
            </div>
        );
    }

    return (
        <section id="instructions">
            <h2>Instructions</h2>
            { instructionElem }
        </section>
    );
}

function viewError () {
    return (
        <div id="wrapper">
            <h1>I could not find that recipe!</h1>

            <p>
              Sorry about that, something went wrong.
            </p>

            <p>
              Please report a bug so that this issue can be fixed.
            </p>
        </div>
    );
}

const params = new URLSearchParams(window.location.search);
const recipeId = decodeURI(params.get('recipeId') || 'no id');

browser.storage.local.get(recipeId).then(recipes => {
    const recipe = recipes[recipeId];
    const node = document.querySelector('#hyperapp');

    console.log('Recipe -> ', recipe);

    if (recipe) {
        document.title = `${recipe.name} :: Recipe Thing`;
        app(recipe, actions, view, node);
    } else {
        app({}, {}, viewError, node);
    }
});
