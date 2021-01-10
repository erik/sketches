import { h } from '../render'

function queryContainerNode (document) {
  return document.querySelector('#bikes .right')
}

function renderError (error) {
  return h('div', {}, [
    h('h1', {}, 'Well this is awkward.'),
    h('p', {}, 'Something went wrong'),
    h('pre', {}, error.toString())
  ])
}

function renderButton () {
  return h('a', {
    className: 'button'
    // onClick: () => renderLinkModal(),
  }, 'Link Bikes')
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
