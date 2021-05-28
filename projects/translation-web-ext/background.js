'use strict';

let popupParams;

browser.menus.create({
  id: 'translate_selection',
  title: 'Translate Selected Text',
  contexts: ["selection"],
});

browser.menus.onClicked.addListener(async (info, tab) => {
  popupParams = {
    tabId: tab.id,
    frameId: info.frameId,
    selection: info.selectionText,
  };

  browser.pageAction.show(tab.id);
  await browser.pageAction.openPopup();
  await browser.pageAction.hide(tab.id);
});


browser.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  console.log('received message:', msg)
  switch (msg) {
  case 'getPopupParams':
    console.log('returning', popupParams)
    sendResponse(popupParams);
  }
});
