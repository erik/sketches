const AppState = {
  athleteId: null,
  rootNode: null,
  gear: {},
  components: {},
}

function queryAthleteId(document) {
  // Convert "/atheletes/id" href to "id"
  return document.querySelector(
    '#athlete-profile a[href^="/athletes/"]'
  )
    .href
    .split('/')
    .pop()
}


function queryContainerNode(document) {
  return document.querySelector(
    '#dashboard-athlete-sidebar .fixed-sidebar-container div'
  )
}


function queryOrCreateRootNode(containerNode) {
  let n = containerNode.querySelector('#gear-ratio-app')
  if (n !== null) {
    return n
  }

  n = createNode('div', {id: 'gear-ratio-app'})
  console.log(containerNode, n)
  containerNode.appendChild(n)
  return n
}


async function initializeState(document) {
  console.log('begin!')
  AppState.athleteId = queryAthleteId(document)

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

function parseTable(table) {
  const data = []
  const columns = Array.from(
    table.rows[0].cells
  ).map(it => it.innerText.trim())

  for (let i = 1; i < table.rows.length; ++i) {
    const row = table.rows[i]
    const rowData = {}

    for (let j = 0; j < row.cells.length; ++j) {
      rowData[columns[j]] = row.cells[j].innerText.trim()
    }

    data.push(rowData)
  }

  return data
}

async function fetchJSON(url) {
  const res = await fetch(url)
  return await res.json()
}

async function fetchHTML(url) {
  const res = await fetch(url)
  const text = await res.text()

  return new DOMParser().parseFromString(text, 'text/html')
}

async function fetchGear(athleteId) {
  const baseURL = `https://www.strava.com/athletes/${athleteId}/gear/`
  const res = {
    bikes: await fetchJSON(baseURL + 'bikes'),
    shoes: await fetchJSON(baseURL + 'shoes'),
  }

  return res
}


async function fetchBikeComponents(gearId) {
  const url = `https://www.strava.com/bikes/${gearId}`
  const doc = await fetchHTML(url)

  console.log('receiv', doc)

  const [
    _, // Bike details, not interested.
    componentTable,
  ] = doc.querySelectorAll('table')

  console.log('parse', componentTable)

  return parseTable(componentTable)
}


function render(state) {
  const bikes = state.gear.bikes.map(bike => {
    const href = `https://strava.com/bikes/${bike.id}`
    const components = state.components[bike.id].map(c => {
      return h('div', {}, [
        h('strong', {}, c.Type),
        '•',
        h('span', {
          title: `Added: ${c.Added}`,
          style: 'border-bottom: 1px dotted #555;',
        }, c.Distance),
      ])
    })
    return h('div', {className: 'text-small'}, [
      h('div', {className: 'text-label'}, h('a', {href}, bike.display_name)),
      h('div', {}, components),
    ])
  })
  const shoes = state.gear.shoes.map(shoe => {
    const href = `https://strava.com/shoes/${shoe.id}`
    return h('div', {className: 'text-small'}, [
      h('div', {
        className: 'text-label'
      }, h('a', {href}, shoe.display_name)),
      h('strong', {}, shoe.total_distance),
    ])
  })

  return h('div', {className: 'card'}, [
    h('div', {className: 'card-section'}, [
      h('h2', {className: 'text-center text-title2 mt-sm mb-md'}, 'Your Gear'),
    ]),
    h('div', {className: 'card-section'}, [
      h('div', {className: 'card-body'}, [
        h('div', {}, bikes),
      ]),
    ]),
    h('div', {className: 'card-section'}, [
      h('div', {className: 'card-body'}, [
        h('div', {}, shoes),
      ]),
    ]),
    h('div', {className: 'card'}, [
      h('div', {className: 'card-footer'}, [
        h('div', {className: 'card-section'}, [
          h('a', {
            className: 'btn-card-link media media-middle',
            href: '/settings/gear',
          }, [
            h('div', {className: 'media-body'}, 'Manage Your Gear'),
            h('div', {className: 'media-right'}, [
              h('span', {className: 'app-icon-wrapper'}, [
                h('span', {className: 'app-icon icon-caret-right icon-dark icon-lg'})
              ])
            ]),
          ])
        ])
      ])
    ])
  ])
}


function createNode(tag, props, children) {
  const ns = props.xmlns || 'http://www.w3.org/1999/xhtml';
  const node = document.createElementNS(ns, tag);

  for (const key in props) {
    const val = props[key];

    switch (key) {
    case 'className':
      node.classList.add(...val.split(' '));
      break;
    case 'onClick':
      node.addEventListener('click', val);
      break;
    default:
      node.setAttribute(key, val);
    }
  }

  children = children || [];
  (Array.isArray(children) ? children : [children])
    .map(ch => (typeof ch === 'string') ? document.createTextNode(ch) : ch)
    .forEach(n => n && node.appendChild(n));

  return node;
}

// shortcuts
createNode.div  = (children) => createNode('div', {}, children);
createNode.p    = (children) => createNode('p', {}, children);
createNode.span = (children) => createNode('span', {}, children);
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
