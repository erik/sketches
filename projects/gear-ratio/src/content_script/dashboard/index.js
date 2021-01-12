import { h } from '../../render'
import { storage } from '../../storage'

const AppState = {
  storage: null,
  athleteId: null,
  gear: {},
  components: {},
  unit: null
}

function queryAthleteId (document) {
  // Convert "/atheletes/id" href to "id"
  return document.querySelector(
    '#athlete-profile a[href^="/athletes/"]'
  )
    .href
    .split('/')
    .pop()
}

function queryContainerNode (document) {
  return document.querySelector(
    '#dashboard-athlete-sidebar .fixed-sidebar-container div'
  )
}

function queryDisplayUnit (document) {
  // FIXME: this is iffy. Some activities (rowing) are hardcoded to
  //   meters even if display preferences are for miles.
  const distanceUnit = (
    document.querySelector('div.activity .stat abbr.unit')
  )
  if (distanceUnit !== null) {
    return distanceUnit.innerText
  }

  return ''
}

function queryOrCreateRootNode (containerNode) {
  let n = containerNode.querySelector('#gear-ratio-app')
  if (n !== null) {
    return n
  }

  n = h('div', { id: 'gear-ratio-app' })
  containerNode.appendChild(n)
  return n
}

async function initializeState (document, storage) {
  AppState.athleteId = queryAthleteId(document)
  AppState.unit = queryDisplayUnit(document)
  AppState.storage = storage

  AppState.gear = await fetchGear(AppState.athleteId)

  for (const bike of AppState.gear.bikes) {
    const components = await fetchBikeComponents(bike.id)
    AppState.components[bike.id] = components
  }

  console.info('initialized app state:', AppState)
}

function parseTable (table) {
  const data = []

  // TODO(erik): hardcoding is bad
  const columnNames = [
    'type',
    'make',
    'model',
    'added',
    'removed',
    'distance',
    'action'
  ]

  // TODO: this provides localized names, should use this
  // const columnNames = Array.from(
  //   table.rows[0].cells
  // ).map(it => it.innerText.trim())

  for (let i = 1; i < table.rows.length; ++i) {
    const row = table.rows[i]
    const rowData = {}

    for (let j = 0; j < row.cells.length; ++j) {
      const name = columnNames[j]
      let val = row.cells[j].innerText.trim()

      // TODO: actually parse this (localized, of course)
      if (name === 'distance') {
        val = val.replace(/[^0-9,. ]/g, '')
      }

      rowData[columnNames[j]] = val
    }

    data.push(rowData)
  }

  return data
}

async function fetchJSON (url) {
  const res = await fetch(url)
  return await res.json()
}

async function fetchHTML (url) {
  const res = await fetch(url)
  const text = await res.text()

  return new DOMParser().parseFromString(text, 'text/html')
}

async function fetchGear (athleteId) {
  const baseURL = `https://www.strava.com/athletes/${athleteId}/gear/`

  const bikes = await fetchJSON(baseURL + 'bikes')
  const shoes = await fetchJSON(baseURL + 'shoes')

  const res = {
    bikes: bikes.filter(it => it.active),
    shoes: shoes.filter(it => it.active)
  }

  return res
}

async function fetchBikeComponents (gearId) {
  const url = `https://www.strava.com/bikes/${gearId}`
  const doc = await fetchHTML(url)

  // Should have two elements:
  //   1. Bike details, not of interest
  //   2. Components
  const tables = doc.querySelectorAll('table')
  return parseTable(tables[1])
}

// TODO: This is BAD.
//  1. render should not be async, state should already be set up
//  2. TOO BIG, split it up.
//  3. linked / unlinked bikes is far too messy, clean it up and dedupe
async function render (state) {
  const distance = (d) => `${d} ${state.unit}`

  const links = await storage.bikeLinks()
  const bikesInLink = new Set()
  Object.values(links).forEach(link => link.bike_ids.forEach(id => bikesInLink.add(id)))

  const unlinkedBikes = state.gear.bikes
  // .filter(bike => !bikesInLink.has(bike.id))
    .map(bike => {
      const href = `https://strava.com/bikes/${bike.id}`
      const isLinked = bikesInLink.has(bike.id)

      const components = state.components[bike.id].map(c => {
        // This implies s is the string "This bike has no active components" (localized)
        if (!c.added) {
          return h('li', {}, c.type)
        }

        return h('li', {
          style: 'display: grid; grid-template-columns: repeat(12, 1fr);'
        }, [
          h('span', {
            style: 'grid-column: 1/8;'
          }, [
            c.type
          ]),
          h('span', { style: 'grid-column: 8/12;text-align:right;', title: c.added }, [
            h('span', { style: 'border-bottom: 1px dotted #777;' }, distance(c.distance))
          ])
        ])
      })

      return h('p', { class: 'text-small' }, [
        h('div', { class: 'text-label' }, [
          h('strong', {}, h('a', { href }, bike.display_name)),
          ' • ',
          h('span', {}, distance(bike.total_distance)),
          isLinked
            ? h('span', { title: 'Linked to xyz' }, (' • 🔗'))
            : null
        ]),
        h('ul', {}, components)
      ])
    })

  const linkedBikes = Object.values(links).map(link => {
    const href = '/settings/gear'

    const sharedComponents = {}
    for (const id of link.bike_ids) {
      const components = state.components[id].filter(c => {
        return link.shared_components.some(shared => shared === c.type)
      })

      components.forEach(c => {
        sharedComponents[c.type] = sharedComponents[c.type] || {}
        sharedComponents[c.type][id] = c
      })
    }

    const components = Object.entries(sharedComponents).map(([type, values]) => {
      // TODO: This isn't a reasonable approach given i18n, see here:
      //  https://observablehq.com/@mbostock/localized-number-parsing
      const combinedDist = Object.values(values)
        .map(v => +v.distance.replace(',', ''))
        .reduce((x, y) => x + y)

      const sourceBikes = link.bike_ids
        .filter(id => typeof sharedComponents[type][id] !== 'undefined')
        .map(id => {
          const bike = state.gear.bikes.find(b => b.id === id)
          const dist = sharedComponents[type][id].distance

          return `${bike.display_name}: ${dist}`
        })
        .join('\n')

      return h('li', {
        style: 'display: grid; grid-template-columns: repeat(12, 1fr);'
      }, [
        h('span', { style: 'grid-column: 1/8;' }, type),
        h('span', { style: 'grid-column: 8/12;text-align:right;' }, [
          h('span', {
            style: 'border-bottom: 1px dotted #777;',
            title: sourceBikes
          }, distance(combinedDist.toLocaleString()))
        ])
      ])
    })

    // XXX: hack
    const totalDistance = link.bike_ids
      .map(id => state.gear.bikes.find(b => b.id === id))
      .map(v => +v.total_distance.replace(',', ''))
      .reduce((x, y) => x + y)

    return h('p', { class: 'text-small' }, [
      h('div', { class: 'text-label' }, [
        h('strong', {}, h('a', { href }, link.name)),
        ' • ',
        h('span', {}, distance(totalDistance.toLocaleString()))
      ]),
      h('ul', {}, components)
    ])
  })

  const shoes = state.gear.shoes.map(shoe => {
    const href = `https://strava.com/shoes/${shoe.id}`
    return h('div', { class: 'text-small' }, [
      h('div', { class: 'text-label' }, [
        h('a', { href }, [
          h('strong', {}, shoe.display_name)
        ]),
        h('span', {}, [
          ' • ',
          distance(shoe.total_distance)
        ])
      ])
    ])
  })

  return h('div', { class: 'card' }, [
    h('div', { class: 'card-body' }, [
      h('div', { class: 'card-section' }, 'Bikes'),
      h('div', { class: 'card-section' }, [
        h('div', {}, unlinkedBikes),
        'Linked', // TODO: better name? Linked is confusing
        h('div', {}, linkedBikes)
      ])
    ]),
    h('div', { class: 'card-body' }, [
      h('div', { class: 'card-section' }, 'Shoes'),
      h('div', { class: 'card-section' }, [
        h('div', {}, shoes)
      ])
    ]),
    renderCardFooter()
  ])
}

function renderCardFooter () {
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

function renderError (error) {
  return h('div', { class: 'card' }, [
    h('div', { class: 'card-body' }, [
      h('div', { class: 'card-section' }, 'Sorry :('),
      h('div', { class: 'card-section' }, [
        h('p', {}, 'Something went wrong'),
        h('p', {}, 'Details:'),
        h('pre', {}, error.toString())
      ])
    ]),
    renderCardFooter()
  ])
}

function renderInitial () {
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
    renderCardFooter()
  ])
}

class App {
  constructor ({
    render,
    node,
    initialState,
    onEvent
  }) {
    this.isQueued = false
    this.isMounted = false

    this.render = render
    this.node = node
    this.state = initialState

    this.eventHandlers = onEvent || {}
    this.queueRender()
  }

  setState (data) {
    this.queueRender()
    const oldState = this.state
    this.state = { ...oldState, ...data }

    this.onEvent('setState', { oldState, newState: this.state })
  }

  onEvent (event, args) {
    const handler = this.eventHandlers[event]
    if (!handler) return

    Promise.resolve()
      .then(() => handler.call(this, args))
      .catch(err => {
        console.exception('UNCAUGHT exception in event handler!', err)
      })
  }

  queueRender () {
    if (this.isQueued) return
    this.isQueued = true

    // Performing this action inside a immediately resolved promise
    // schedules the `.then` to be executed after all non-async work.
    //
    // Called a micro-task.
    Promise.resolve().then(async () => {
      const rendered = await this.render(
        this.state,
        this.setState
      )
      this.node.replaceChildren(rendered)

      this.isQueued = false
      if (!this.isMounted) {
        this.isMounted = true
        this.onEvent('mounted')
      }

      this.onEvent('render')
    }).catch(error => {
      this.onEvent('error', { error })
    })
  }
}

(async () => {
  const containerNode = queryContainerNode(document)
  const rootNode = queryOrCreateRootNode(containerNode)

  const app = new App({
    render: async (state, setState) => {
      if (state.isError) {
        return renderError(state.error)
      }

      if (state.isLoading) {
        return renderInitial()
      }

      // TODO: remove this hack
      return await render(state.hack)
    },
    node: rootNode,
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
          await storage.applyMigrations()
          await initializeState(document, storage)

          this.setState({
            isLoading: false,
            hack: AppState
          })
        } catch (error) {
          this.onEvent('error', { error })
        }
      },

      setState ({ newState }) {
        console.info('persisting new state:', newState)
      },

      error ({ error }) {
        console.exception('CAUGHT Exception in app', error)
        this.setState({ isError: true, error })
      }
    }
  })
})()
