import { h } from '../render'

function queryContainerNode (document) {
  return document.querySelector('#bikes .right')
}

function renderButton () {
  return h('a', {
    class: 'button',
    onClick: () => renderLinkModal()
  }, 'Link Bikes')
}

function renderLinkModal () {
  // TODO: include render state, don't just append child
  const body = document.body

  const background = h('div', { class: 'ui-widget-overlay ui-front' })
  const modal = h('div', { class: 'ui-dialog ui-widget ui-widget-content ui-corner-all ui-front' }, [
    h('div', { class: 'ui-dialog-titlebar ui-widget-header ui-corner-all ui-helper-clearfix' }, [
      h('span', { class: 'ui-dialog-title' }, 'Link Bikes')
    ]),
    h('div', { class: 'ui-dialog-content ui-widget-content' }, [
      h('form', { novalidate: 'novalidate' }, [
        'hi :)'
      ])
    ])
  ])

  body.appendChild(background)
  body.appendChild(modal)
}

(async () => {
  console.log('start')

  try {
    const container = queryContainerNode(document)
    container.prepend(renderButton())
  } catch (err) {
    console.exception(err)
  }
})()
