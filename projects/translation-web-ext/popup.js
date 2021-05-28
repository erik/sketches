'use strict';

const LINGUEE_URL = 'https://www.linguee.com/english-german/search?source=auto&query=';

(async () => {
  const params = await browser.runtime.sendMessage('getPopupParams');
  const { selection } = params;

  const res = await fetch(LINGUEE_URL + window.encodeURIComponent(selection));
  const html = await res.text();

  const parser = new DOMParser();
  const doc = parser.parseFromString(html, 'text/html');

  const style = doc.querySelector('style');
  const node = doc.querySelector('#dictionary');

  const output = document.getElementById('popup-output');
  output.appendChild(style);
  output.appendChild(node);
})();
