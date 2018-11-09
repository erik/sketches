'use strict';

browser = typeof 'browser' === 'undefined' ? chrome : browser;

const CAPTURED_URLS = [
  'https://fonts.googleapis.com/*',
  'http://fonts.googleapis.com/*',
  'https://fonts.gstatic.com/*',
  'http://fonts.gstatic.com/*',
];

// TODO: Pick something more general.
const DEFAULT_FACELESS_UA = 'typefaceless';
const DEFAULT_FACELESS_REFERRER = 'https://google.com';

function rewriteHeaders(rewriteRules) {
  return function(evt) {
    for (let header of evt.requestHeaders) {
      const value = rewriteRules[header.name.toLowerCase()];

      if (typeof value !== 'undefined') {
        header.value = value;
      }
    }

    browser.runtime.sendMessage({
      type: 'request.rewritten',
      host: 'todo',
    });

    return {requestHeaders: evt.requestHeaders};
  }
}

browser.storage.get('typefaceless.options')
  .then((options) => {
    const rewriteRules = {
      'user-agent': options['faceless_ua'] || DEFAULT_FACELESS_UA,
      'referer': options['faceless_referrer'] || DEFAULT_FACELESS_REFERRER,
    };

    browser.webRequest.onBeforeSendHeaders.addListener(
      rewriteHeaders(rewriteRules),
      {urls: CAPTURED_URLS},
      ['blocking', 'requestHeaders']);
  });

browser.runtime.onMessage.addListener((msg, sender) => {
  const tabId = sender.tab.id;

  switch (msg.type) {
  case 'request.rewritten':
    // TODO: update metrics
    // TODO: enable icon
    break;
  }
});


// browser.tabs.onChanged() ->
//   handle switching tabs, maintaining state between tabs
