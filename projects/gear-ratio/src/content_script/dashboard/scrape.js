import { h } from '../../render.js'

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
    // TODO!
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
  async fetchGear (athleteId) {
    const baseURL = `https://www.strava.com/athletes/${athleteId}/gear/`

    const bikes = await fetchJSON(baseURL + 'bikes')
    const shoes = await fetchJSON(baseURL + 'shoes')

    const res = {
      bikes: bikes.filter(it => it.active),
      shoes: shoes.filter(it => it.active)
    }

    return res
  },

  async fetchBikeComponents (gearId) {
    const url = `https://www.strava.com/bikes/${gearId}`
    const doc = await fetchHTML(url)

    // Should have two elements:
    //   1. Bike details, not of interest
    //   2. Components
    const tables = doc.querySelectorAll('table')
    return parseBikeTable(tables[1])
  }
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

export default {
  dashboard: DashboardScraper,
  gear: GearScraper
}
