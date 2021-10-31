'use strict';

async function fetchLinguee(langPair, query, sourceLang = 'auto') {
  const encoded = window.encodeURIComponent(query);

  const res = await fetch(
    `https://www.linguee.com/${langPair}/search?source=${sourceLang}&query=${encoded}`
  );

  // Can't use res.text() directly, since that always assumes UTF-8,
  // which isn't true here.
  const bytes = await res.arrayBuffer();
  const html = new TextDecoder('iso-8859-15').decode(bytes);

  const doc = new DOMParser().parseFromString(html, 'text/html');

  const style = doc.querySelector('style');
  const dictEntry = doc.querySelector('.isForeignTerm');

  const node = document.createElement('div');
  node.id = 'dictionary';

  node.appendChild(style);
  node.appendChild(dictEntry);

  return node;
}

async function fetchDeepl(langPair, query, sourceLang = 'auto') {
  // TODO: add this for translating full sentences.
}

function pushHistory(query, type) {
  // TODO: write me
}

(async () => {
  const params = await browser.runtime.sendMessage('getPopupParams');
  const { selection } = params;

  const output = document.getElementById('lingueecontent');

  output.appendChild(await fetchLinguee('english-german', selection));
})();
