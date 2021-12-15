'use strict';


const BASE_URL = 'https://www.dwds.de';

class DwdsClient {
  constructor (baseUrl = BASE_URL) {
    this.baseUrl = baseUrl;
  }

  async get(path, query) {
  }

  // Return frequency of lemma within DWDS' corpus. Log scale between
  // 0 (least) and 6 (most)
  async wordFrequency (lemma) {
    const response = await this.get('/api/frequency', { q: lemma });
    const body = await response.json();
    return body.frequency;
  }

  // Look up word information (can provide multiple)
  async wordInformation (...words) {
    const response = await this.get('/api/wb/snippet', { q: words.join('|') });
    const body = await response.json();

    return body.map(({url, input, wordart, lemma}) => ({
      url,
      lemma,
      word: input,
      kind: wortart,
    }));
  }
}

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
