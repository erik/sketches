import { h } from '../../render'
import { App } from '../../app'

function queryAppRootNode (document) {
  const container = document.querySelector('#bikes .right')
  const existingNode = container.querySelector('#app-gear-index')
  if (existingNode) {
    return existingNode
  }

  const node = h('div', { id: 'app-gear-index', style: 'display: inline-block;' })
  container.prepend(node)
  return node
}

const ModalFormInput = ({ onSaveLink }) => {
  const onSubmit = (el) => {
    el.stopPropagation()
    el.preventDefault()

    const formData = new FormData(el.target.form)
    const linkData = {}

    formData.forEach((val, key) => { linkData[key] = val })
    onSaveLink(linkData)
  }

  const textLabel = (label, name) => h('span', {}, [
    h('label', { for: name }, label),
    h('input', { id: name, class: 'medium', type: 'text' })
  ])

  return h('div', {}, [
    textLabel('Name', 'link_name'),
    textLabel('Linked Bikes', 'bikes'),
    textLabel('Shared Components', 'components'),
    h('br', {}),

    // HACK: Strava's got some jQuery thing going on which overrides
    //   any click handlers we set on submit buttons. So create a parent
    //   node and stop propagation.
    h('div', { onClick: onSubmit },
      h('input', { type: 'submit', value: 'Save Link' })
    )
  ])
}

const LinkBikesModal = ({ onCloseModal, onSaveLink }) => {
  const background = h('div', {
    class: 'ui-widget-overlay ui-front',
    onClick: onCloseModal
  })

  const modal = h('div', { class: 'ui-dialog ui-widget ui-widget-content ui-corner-all ui-front' }, [
    h('div', { class: 'ui-dialog-titlebar ui-widget-header ui-corner-all ui-helper-clearfix' }, [
      h('span', { class: 'ui-dialog-title' }, 'Link Bikes'),
      h('p', {}, [
        'Select two or more existing bikes to treat as a single unit.'
      ])
    ]),
    h('div', {
      class: 'ui-dialog-content ui-widget-content',
      style: 'display: block; width: auto; height: auto;'
    }, [
      h('form', { novalidate: 'novalidate' }, [
        h(ModalFormInput, { onSaveLink })
      ])
    ])
  ])

  return h('div', {}, [
    background,
    modal
  ])
}

(async () => {
  try {
    const app = new App({
      initialState: {
        isModalVisible: false
      },

      render () {
        // FIXME: big ol' hack that this is inside App.render.
        const onCloseModal = () => this.setState({ isModalVisible: false })
        const onOpenModal = () => this.setState({ isModalVisible: true })
        const onSaveLink = (link) => {
          console.log('READY TO SAVE', link)
          onCloseModal()
        }

        return h('div', {}, [
          this.state.isModalVisible && h(LinkBikesModal, { onCloseModal, onSaveLink }),
          h('a', { class: 'button', onClick: onOpenModal }, 'Link Bikes')
        ])
      },

      onEvent: {
        async mounted () {
          console.log('app mount')
        },

        error ({ error }) {
          console.exception('CAUGHT Exception in app', error)
          this.setState({ isError: true, error })
        }
      }
    })

    const rootNode = queryAppRootNode(document)
    app.mount(rootNode)
  } catch (err) {
    console.exception('Error!', err)
  }
})()
