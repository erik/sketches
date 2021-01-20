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
  // Component-local state that DOES NOT trigger a re-render.
  //
  // This is getting hacky...
  const state = {
    linkName: '',
    includedBikeIds: new Set()
  }

  const onSubmit = (el) => {
    el.stopPropagation()
    el.preventDefault()

    const linkName = state.linkName.trim()
    if (linkName === '') {
      window.alert('Please give this configuration a name')
      return
    }

    if (state.includedBikeIds.size === 0) {
      window.alert('Please select at least 1 existing bike to proceed')
      return
    }

    const linkData = {
      linkName,
      includedBikeIds: state.includedBikeIds
    }

    onSaveLink(linkData)
  }

  const textLabel = ({ label, name, onInput }) => h('span', {}, [
    h('label', { for: name }, label),
    h('input', {
      name,
      onInput,
      id: name,
      required: true,
      autocomplete: 'off',
      class: 'large',
      type: 'text'
    })
  ])

  const displayInline = { style: 'display: inline-block;' }

  const includedBikeList = gear.bikes.map(b => {
    const id = `bike_${b.id}`
    return h('li', {}, [
      h('input', {
        id,
        class: 'small',
        name: id,
        type: 'checkbox',
        onClick: (ch) => {
          if (ch.target.checked) {
            state.includedBikeIds.add(b.id)
          } else {
            state.includedBikeIds.delete(b.id)
          }
        },
        ...displayInline
      }),
      ' ',
      h('label', { for: id, ...displayInline }, h('b', {}, b.display_name))
    ])
  })

  return h('div', {}, [
    textLabel({
      label: 'Name',
      name: 'linkName',
      onInput: (i) => { state.linkName = i.target.value }
    }),
    h('p', {}, 'Included Bikes'),
    h('ul', {}, includedBikeList),
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
            title: buttonEnabled ? '' : 'Please wait, refreshing data',
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
