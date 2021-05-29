'use strict';

let popupParams;

browser.menus.create({
  id: 'translate_selection',
  title: 'Translate Selected Text',
  contexts: ["selection"],
});

browser.menus.onClicked.addListener(async (info, tab) => {
  await showTranslationPopup(info.selectionText, tab);
});

browser.browserAction.onClicked.addListener(async (tab) => {
  // We have to do these first as the first async call will take us
  // out of the "user input" context we need in order to open the
  // popup.
  browser.pageAction.show(tab.id);
  browser.pageAction.openPopup();

  const selections = await browser.tabs.executeScript(
    tab.id,
    {code: '(window.getSelection().toString())', allFrames: true}
  );

  const selected = selections.find(it => !!it);
  if (!selected) {
    return;
  }

  await showTranslationPopup(selected, tab);
});



async function showTranslationPopup(text, tab) {
  popupParams = {
    tabId: tab.id,
    selection: text,
  };

  browser.pageAction.show(tab.id);
  await browser.pageAction.openPopup();
  await browser.pageAction.hide(tab.id);
}


browser.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  switch (msg) {
  case 'getPopupParams':
    sendResponse(popupParams);
  }
});
