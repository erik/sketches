import { h } from '../../render'
import { App } from '../../app'

import scrape from '../scrape.js'

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

function queryAthleteId (document) {
  const node = document.querySelector('a.nav-link[href^="/athletes/"]')
  return node.href
    .split('/')
    .pop()
}

const LoadingSpinner = () => {
  return h('div', { class: 'spinner tiny' }, h('div', { class: 'graphic' }))
}

const ModalFormInput = ({ gear, onSaveLink }) => {
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
    h('input', { id: name, name, required: true, class: 'large', type: 'text' })
  ])

  const inline = { style: 'display: inline-block;' }

  return h('div', {}, [
    textLabel('Name', 'linkName'),
    h('p', {}, 'Included Bikes'),
    h('ul', {}, gear.bikes.map(b => {
      const id = `bike_${b.id}`
      return h('li', {}, [
        h('input', { id, class: 'medium', name: id, type: 'checkbox', ...inline }),
        ' ',
        h('label', { for: id, ...inline }, h('b', {}, b.display_name))
      ])
    })),
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

const LinkBikesModal = ({ gear, onCloseModal, onSaveLink }) => {
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
    }, h('form', { novalidate: 'novalidate' }, [
      h(ModalFormInput, { gear, onSaveLink })
    ]))
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
        isLoadingGear: true,
        isModalVisible: false,

        gear: {
          bikes: [],
          shoes: [],
          bikeComponnents: []
        }
      },

      render () {
        // FIXME: big ol' hack that this is inside App.render.
        const onCloseModal = () => this.setState({ isModalVisible: false })
        const onOpenModal = () => this.setState({ isModalVisible: true })
        const onSaveLink = (link) => {
          // TODO: impl
          console.log('READY TO SAVE', link)
          onCloseModal()
        }

        const buttonEnabled = !this.state.isLoadingGear
        const buttonClassList = buttonEnabled
          ? 'button'
          : 'button disabled'

        return h('div', {}, [
          this.state.isModalVisible && h(LinkBikesModal, {
            onCloseModal,
            onSaveLink,
            gear: this.state.gear
          }),
          h('a', {
            class: buttonClassList,
            title: buttonEnabled ? '' : 'Loading',
            onClick: buttonEnabled && onOpenModal
          }, [
            'Link Bikes'
          ])
        ])
      },

      onEvent: {
        async mounted () {
          console.log('app mount')
          const athleteId = queryAthleteId(document)
          const gear = await scrape.gear.refreshGear(athleteId)

          this.setState({ isLoadingGear: false, gear })
        },

        error ({ error }) {
          console.exception('CAUGHT Exception in app', error)
          // TODO: not using this right now, and this can cause
          // recursive rendering errors (lol)

          // this.setState({ isError: true, error })
        }
      }
    })

    const rootNode = queryAppRootNode(document)
    app.mount(rootNode)
  } catch (err) {
    console.exception('Error!', err)
  }
})()
