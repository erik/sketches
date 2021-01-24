import { h } from '../../render'
import { App } from '../../app'
import { persistentState } from '../../persist.js'

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

const formTextInput = ({ labelText, id, onInput }) => h('span', {}, [
  h('label', { for: id }, labelText),
  h('input', {
    id,
    onInput,

    name: id,
    required: true,
    autocomplete: 'off',
    class: 'large',
    type: 'text'
  })
])

const ModalBikesFormInput = ({ gear, onClickNext }) => {
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

    const name = state.linkName.trim()
    if (name === '') {
      window.alert('Please give this configuration a name')
      return
    }

    if (state.includedBikeIds.size === 0) {
      window.alert('Please select at least 1 existing bike to proceed')
      return
    }

    const linkData = {
      name,
      bikeIds: state.includedBikeIds
    }

    onClickNext(linkData)
  }

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
    formTextInput({
      id: 'linkName',
      labelText: 'Name',
      onInput: (i) => { state.linkName = i.target.value }
    }),
    h('p', {}, 'Included Bikes'),
    h('ul', {}, includedBikeList),

    // HACK: Strava's got some jQuery thing going on which overrides
    //   any click handlers we set on submit buttons. So create a parent
    //   node and stop propagation.
    h('div', { onClick: onSubmit },
      h('input', { type: 'submit', value: 'Choose Components' })
    )
  ])
}

const ModalComponentsFormInput = ({ linkBikeIds, linkName, gear, onClickNext }) => {
  const state = {
    componentTypes: new Set()
  }

  const onSubmit = (el) => {
    el.stopPropagation()
    el.preventDefault()

    if (state.componentTypes.size === 0) {
      window.alert('Please select at least 1 component type to share')
      return
    }

    onClickNext(state.componentTypes)
  }

  const displayInline = { style: 'display: inline-block;' }
  console.log('my gear is', gear, linkBikeIds)

  const componentsByType = {}
  for (const bikeId of linkBikeIds) {
    for (const component of gear.bikeComponents[bikeId]) {
      const type = component.type
      componentsByType[type] = componentsByType[type] || []
      componentsByType[type].push(bikeId)
    }
  }

  const checkbox = (type, bikeIds) => {
    return h('li', {}, [
      h('input', {
        class: 'small',
        name: type,
        type: 'checkbox',
        onClick: (ch) => {
          if (ch.target.checked) {
            state.componentTypes.add(type)
          } else {
            state.componentTypes.delete(type)
          }
        },
        ...displayInline
      }),
      ' ',
      h('label', { for: type, ...displayInline }, [
        h('b', {}, type),
        h('small', {}, [
          '(', bikeIds.length, ' configurations)'
        ])
      ])
    ])
  }

  const includedComponentsList = Object.entries(componentsByType)
    .sort()
    .map(([type, bikeIds]) => checkbox(type, bikeIds))

  return h('div', {}, [
    h('p', {}, [
      'Which components are shared across all configurations of ',
      h('b', {}, linkName),
      '?'
    ]),
    h('ul', {}, includedComponentsList),
    h('div', { onClick: onSubmit },
      h('input', { type: 'submit', value: 'Create' })
    )
  ])
}

const Modal = ({ onClose, children }) => {
  const background = h('div', {
    class: 'ui-widget-overlay ui-front',
    onClick: onClose
  })

  const modal = h('div', { class: 'ui-dialog ui-widget ui-widget-content ui-corner-all ui-front' }, [
    h('div', { class: 'ui-dialog-titlebar ui-widget-header ui-corner-all ui-helper-clearfix' }, [
      h('span', { class: 'ui-dialog-title' }, 'Link Bikes')
    ]),
    h('div', {
      class: 'ui-dialog-content ui-widget-content',
      style: 'display: block; width: auto; height: auto;'
    }, h('form', { novalidate: 'novalidate' }, children))
  ])

  return h('div', {}, [
    background,
    modal
  ])
}

async function persistLink (link) {
  const state = await persistentState.restore()

  const bikeLinks = state.bikeLinks || []
  bikeLinks.push(link)

  await persistentState.persist({ bikeLinks })
}

(async () => {
  try {
    const app = new App({
      initialState: {
        isLoadingGear: true,
        isModalVisible: false,

        // bikes | components
        modalStep: 'bikes',

        link: {
          name: null,
          bikeIds: [],
          componentTypes: []
        },

        gear: {
          bikes: [],
          shoes: [],
          bikeComponents: []
        }
      },

      render () {
        // FIXME: big ol' hack that this is inside App.render.
        const onCloseModal = () => {
          this.setState({
            isModalVisible: false
          })
        }

        const onOpenModal = () => {
          this.setState({
            modalStep: 'bikes',
            isModalVisible: true
          })
        }

        let modalContents
        switch (this.state.modalStep) {
          case 'bikes':
            modalContents = h(ModalBikesFormInput, {
              gear: this.state.gear,
              onClickNext: (link) => {
                this.setState({
                  modalStep: 'components',
                  link: { componentTypes: [], ...link }
                })
              }
            })
            break

          case 'components':
            modalContents = h(ModalComponentsFormInput, {
              linkName: this.state.link.name,
              linkBikeIds: this.state.link.bikeIds,
              gear: this.state.gear,
              onClickNext: async (componentTypes) => {
                const link = { ...this.state.link, componentTypes }

                this.setState({ link })
                await persistLink(link)

                onCloseModal()
              }
            })
            break

          default:
            console.warn('Unknown modal step??', this.state.modalStep)
        }

        const buttonEnabled = !this.state.isLoadingGear
        const buttonClassList = buttonEnabled
          ? 'button'
          : 'button disabled'

        return h('div', {}, [
          this.state.isModalVisible && h(Modal, {
            onClose: onCloseModal,
            children: modalContents
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
          const locale = scrape.locale(document)

          const gear = await scrape.gear.refreshGear(athleteId, locale)

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
