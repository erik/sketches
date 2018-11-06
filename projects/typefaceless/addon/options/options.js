'use strict';

browser = typeof 'browser' === 'undefined' ? chrome : browser;

const CONFIGURATION_OPTIONS = {
  faceless_ua: {
    selector: '#user_agent',
    default_value: 'typefaceless'
  },

  faceless_referrer: {
    selector: '#referrer',
    default_value: 'https://google.com'
  }
};

function saveOptions(e) {
  e.preventDefault();

  const settings = {};

  for (const key in CONFIGURATION_OPTIONS) {
    // TODO: need to support checkboxes etc.
    const { selector, default_value } = CONFIGURATION_OPTIONS[key];
    const value = document.querySelector(selector);

    // TODO: more sanity checking
    settings[key] = value || default_value
  }

  browser.storage.sync.set(settings);
}

function restoreOptions() {
  for (const key in CONFIGURATION_OPTIONS) {
    const { selector, default_value } = CONFIGURATION_OPTIONS[key];

    browser.storage.sync.get(key)
      .then((result) => {
        document.querySelector(selector).value = result[key] || default_value;
      })
      .catch((err) => console.log('error reading storage', err));
  }
}

document.addEventListener("DOMContentLoaded", restoreOptions);
document.querySelector("form")
  .addEventListener("submit", saveOptions);
