import * as model from '../../model.js'
import { App } from '../../app.js'
import { h } from '../../render.js'
import { persistentState } from '../../persist.js'

import scrape from '../scrape.js'

const GlobalState = {
  athleteId: null,
  distanceUnit: null,
  locale: null,

  async initialize (doc) {
    this.athleteId = scrape.dashboard.athleteId(doc)
    this.distanceUnit = scrape.dashboard.displayUnit(doc)
    this.locale = scrape.locale(doc)
  }
}

const formatDistance = (val) => `${val.toLocaleString()} ${GlobalState.distanceUnit}`

const BikeComponent = ({ href, type, added, distance }) => {
  // This implies s is the string "This bike has no active components" (localized)
  if (!added) {
    return h('li', {}, type)
  }

  return h('li', {
    style: 'display: grid; grid-template-columns: repeat(12, 1fr);'
  }, [
    h('span', { style: 'grid-column: 1/8;' }, h('a', { href }, type)),
    h('span', { style: 'grid-column: 8/12; text-align:right;', title: added }, [
      h('span', { style: 'border-bottom: 1px dotted #777;' }, formatDistance(distance))
    ])
  ])
}

const Bike = ({ bike, components }) => {
  const href = `https://strava.com/bikes/${bike.id}`
  const componentList = components.map(c => h(BikeComponent, { ...c }))

  return h('p', { class: 'text-small' }, [
    h('div', { class: 'text-label' }, [
      h('strong', {}, h('a', { href }, bike.display_name)),
      ' • ',
      h('span', {}, formatDistance(bike.total_distance))
    ]),
    h('ul', {}, componentList)
  ])
}

const BikeLink = ({ link }) => {
  const href = 'https://strava.com/todo'

  return h('p', { class: 'text-small' }, [
    h('div', { class: 'text-label' }, [
      h('strong', {}, h('a', { href }, ['🔗', link.name])),
      ' • ',
      h('span', {}, 'todo: total distance')
    ]),
    h('ul', {}, 'todo: components')
  ])
}

const Shoe = ({ shoe }) => {
  const href = `https://strava.com/shoes/${shoe.id}`

  return h('div', { class: 'text-small' }, [
    h('div', { class: 'text-label' }, [
      h('a', { href }, h('strong', {}, shoe.display_name)),
      h('span', {}, [' • ', formatDistance(shoe.total_distance)])
    ])
  ])
}

const GearCard = {
  props: [
    'shoes',
    'bikes',
    'bikeLinks',
    'bikeComponents'
  ],

  render () {
    const bikes = this.bikes.map(bike => {
      const components = this.bikeComponents[bike.id] || []
      return h(Bike, { bike, components })
    })

    // TODO: better rendering
    const bikeLinks = (this.bikeLinks || []).map(link => h(BikeLink, { link }))

    const shoes = this.shoes.map(shoe => h(Shoe, { shoe }))

    return h('div', { class: 'card' }, [
      h('div', { class: 'card-body' }, [
        h('div', { class: 'card-section' }, 'Bikes'),
        h('div', { class: 'card-section' }, [
          h('div', {}, bikes),
          h('div', {}, bikeLinks)
        ])
      ]),
      h('div', { class: 'card-body' }, [
        h('div', { class: 'card-section' }, 'Shoes'),
        h('div', { class: 'card-section' }, [
          h('div', {}, shoes)
        ])
      ]),
      h(CardFooter)
    ])
  }
}

const CardFooter = () => {
  return h('div', { class: 'card-footer' }, [
    h('div', { class: 'card-section' }, [
      h('a', {
        class: 'btn-card-link media media-middle',
        href: '/settings/gear'
      }, [
        h('div', { class: 'media-body' }, 'Manage Your Gear'),
        h('div', { class: 'media-right' }, [
          h('span', { class: 'app-icon-wrapper' }, [
            h('span', { class: 'app-icon icon-caret-right icon-dark icon-lg' })
          ])
        ])
      ])
    ])
  ])
}

const ErrorCard = ({ error }) => {
  return h('div', { class: 'card' }, [
    h('div', { class: 'card-body' }, [
      h('div', { class: 'card-section' }, 'Sorry :('),
      h('div', { class: 'card-section' }, [
        h('p', {}, 'Something went wrong'),
        h('p', {}, 'Details:'),
        h('pre', {}, error.toString())
      ])
    ]),
    h(CardFooter)
  ])
}

const LoadingCard = () => {
  return h('div', { class: 'card' }, [
    h('div', { class: 'card-body' }, [
      h('div', { class: 'card-section' }, [
        h('div', { class: 'loading-container' }, [
          h('div', { class: 'spinner sm' }, [
            h('div', { class: 'graphic' }),
            h('span', { class: 'status' }, 'Loading ...')
          ])
        ])
      ])
    ]),
    h(CardFooter)
  ])
}

(async () => {
  const app = new App({
    render () {
      if (this.state.isError) {
        return h(ErrorCard, { error: this.state.error })
      }

      if (this.state.isLoading) {
        return h(LoadingCard, {})
      }

      return h(GearCard, {
        shoes: this.state.shoes,
        bikes: this.state.bikes,
        bikeComponents: this.state.bikeComponents,
        bikeLinks: this.state.bikeLinks
      })
    },

    initialState: {
      isLoading: true,
      isError: false,
      error: null
    },

    onEvent: {
      async mounted () {
        console.info('restoring state')

        // TODO: clean way of wrapping errors here? (and in event
        //   handlers in general)
        try {
          await GlobalState.initialize(document)
          let appState = await persistentState.restore()

          // Since refreshing gear involves a bit of fanout (one call per component per bike),
          // let's limit it to once / 15 min. Should speed up repeat renders.
          //
          // TODO: Might be nice to have initial render with cached
          //   values and then load in refreshed ones in the background
          //   since these do not change often.
          //
          // TODO: Add cache buster for debugging / development
          //
          // TODO: Turn app state into real class
          const forceRefresh = window.location.search.includes('forceRefresh')
          const FETCH_INTERVAL_MS = 15 * 60 * 1000
          if (
            forceRefresh ||
              !appState.lastFetchedAt ||
              Math.abs(new Date() - appState.lastFetchedAt) > FETCH_INTERVAL_MS
          ) {
            appState = {
              ...appState,
              ...await scrape.gear.refreshGear(GlobalState.athleteId, GlobalState.locale)
            }
          }

          this.setState({ isLoading: false, ...appState })
        } catch (error) {
          this.onEvent('error', { error })
        }
      },

      async setState ({ newState }) {
        console.info('persisting new state:', newState)

        await persistentState.persist({
          bikes: newState.bikes,
          shoes: newState.shoes,
          bikeComponents: newState.bikeComponents,
          lastFetchedAt: newState.lastFetchedAt
        })
      },

      error ({ error }) {
        console.exception('CAUGHT Exception in app', error)
        this.setState({ isError: true, error })
      }
    }
  })

  const rootNode = scrape.dashboard.appRootNode(document)
  app.mount(rootNode)
})()
