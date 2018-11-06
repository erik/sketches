'use strict';

browser = typeof 'browser' === 'undefined' ? chrome : browser;

const CAPTURED_URLS = [
  'https://fonts.googleapis.com/*',
  'http://fonts.googleapis.com/*',
  'https://fonts.gstatic.com/*',
  'http://fonts.gstatic.com/*',
];

// TODO: Pick something more general.
const FACELESS_UA = 'typefaceless';
const FACELESS_REFERRER = 'https://google.com';

const HEADER_REWRITE_RULES = {
  'user-agent': FACELESS_UA,
  'referer': FACELESS_REFERRER,
};

function rewriteHeaders(e) {
  for (let header of e.requestHeaders) {
    let value = HEADER_REWRITE_RULES[header.name.toLowerCase()];
    if (typeof value !== 'undefined') {
      header.value = value;
    }
  }

  return {
    requestHeaders: e.requestHeaders
  };
}

browser.webRequest.onBeforeSendHeaders.addListener(
  rewriteHeaders,
  {urls: CAPTURED_URLS},
  ['blocking', 'requestHeaders']);
