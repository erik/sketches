import { h } from '../render.js'

// Everything here munges HTML on www.strava.com/dashboard
export const DashboardScraper = {
  athleteId (document) {
    // Convert "/atheletes/id" href to "id"
    return document.querySelector(
      '#athlete-profile a[href^="/athletes/"]'
    )
      .href
      .split('/')
      .pop()
  },

  locale (document) {
    return document.documentElement.lang || 'en'
  },

  displayUnit (document) {
    // FIXME: this is iffy. Some activities (rowing) are hardcoded to
    //   meters even if display preferences are for miles.
    const distanceUnit = (
      document.querySelector('div.activity .stat abbr.unit')
    )
    if (distanceUnit !== null) {
      return distanceUnit.innerText
    }

    return ''
  },

  appRootNode (document) {
    const container = document.querySelector(
      '#dashboard-athlete-sidebar .fixed-sidebar-container div'
    )

    let n = container.querySelector('#gear-ratio-app')
    if (n !== null) {
      return n
    }

    n = h('div', { id: 'gear-ratio-app' })
    container.appendChild(n)
    return n
  }
}

const GearScraper = {
  async refreshGear (athleteId) {
    console.info('Refreshing gear data.')
    const { bikes, shoes } = await fetchGear(athleteId)
    const bikeComponents = {}

    for (const bike of bikes) {
      console.info('Refreshing components for bike', bike)
      bikeComponents[bike.id] = await fetchBikeComponents(bike.id)
    }

    return {
      bikes,
      bikeComponents,
      shoes,

      lastFetchedAt: new Date()
    }
  }

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
  return parseBikeTable(tables[1])
}

// TODO: this could be easily generalized.
function parseBikeTable (table) {
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
      const node = row.cells[j]
      let textVal = node.innerText.trim()

      if (name === 'type') {
        const editLink = node.querySelector('a[href^="/components/"]')
        if (editLink !== null) {
          rowData.href = editLink.href
        }
      } else if (name === 'distance') {
        // TODO: actually parse this (localized, of course)
        textVal = textVal.replace(/[^0-9,. ]/g, '')
      }

      rowData[columnNames[j]] = textVal
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

export default {
  dashboard: DashboardScraper,
  gear: GearScraper
}
