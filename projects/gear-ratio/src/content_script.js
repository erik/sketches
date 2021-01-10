const AppState = {
  athleteId: null,
  rootNode: null,
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

  n = createNode('div', { id: 'gear-ratio-app' })
  console.log(containerNode, n)
  containerNode.appendChild(n)
  return n
}

async function initializeState (document) {
  console.log('begin!')
  AppState.athleteId = queryAthleteId(document)
  AppState.unit = queryDisplayUnit(document)

  const containerNode = queryContainerNode(document)
  AppState.rootNode = queryOrCreateRootNode(containerNode)

  console.log(AppState)

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

  console.log('receiv', doc)

  const [
    _, // Bike details, not interested.
    componentTable
  ] = doc.querySelectorAll('table')

  console.log('parse', componentTable)

  return parseTable(componentTable)
}

function render (state) {
  const distance = (d) => `${d} ${state.unit}`

  const bikes = state.gear.bikes.map(bike => {
    const href = `https://strava.com/bikes/${bike.id}`

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

    return h('p', { className: 'text-small' }, [
      h('div', { className: 'text-label' }, [
        h('strong', {}, h('a', { href }, bike.display_name)),
        ' • ',
        h('span', {}, distance(bike.total_distance))
      ]),
      h('ul', {}, components)
    ])
  })
  const shoes = state.gear.shoes.map(shoe => {
    const href = `https://strava.com/shoes/${shoe.id}`
    return h('div', { className: 'text-small' }, [
      h('div', { className: 'text-label' }, [
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

  return h('div', { className: 'card' }, [
    h('div', { className: 'card-body' }, [
      h('div', { className: 'card-section' }, 'Bikes'),
      h('div', { className: 'card-section' }, [
        h('div', {}, bikes)
      ])
    ]),
    h('div', { className: 'card-body' }, [
      h('div', { className: 'card-section' }, 'Shoes'),
      h('div', { className: 'card-section' }, [
        h('div', {}, shoes)
      ])
    ]),
    h('div', { className: 'card-footer' }, [
      h('div', { className: 'card-section' }, [
        h('a', {
          className: 'btn-card-link media media-middle',
          href: '/settings/gear'
        }, [
          h('div', { className: 'media-body' }, 'Manage Your Gear'),
          h('div', { className: 'media-right' }, [
            h('span', { className: 'app-icon-wrapper' }, [
              h('span', { className: 'app-icon icon-caret-right icon-dark icon-lg' })
            ])
          ])
        ])
      ])
    ])
  ])
}

function createNode (tag, props, children) {
  const ns = props.xmlns || 'http://www.w3.org/1999/xhtml'
  const node = document.createElementNS(ns, tag)

  for (const key in props) {
    const val = props[key]

    switch (key) {
      case 'className':
        node.classList.add(...val.split(' '))
        break
      case 'onClick':
        node.addEventListener('click', val)
        break
      default:
        node.setAttribute(key, val)
    }
  }

  children = children || [];
  (Array.isArray(children) ? children : [children])
    .map(ch => (typeof ch === 'string') ? document.createTextNode(ch) : ch)
    .forEach(n => n && node.appendChild(n))

  return node
}

// shortcuts
const h = createNode;

(async () => {
  console.log('start')
  try {
    await initializeState(document)
    console.log('did it do')
    AppState.rootNode.replaceChildren(render(AppState))
  } catch (err) {
    console.exception('failed with', err)
  }
})()
