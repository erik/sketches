import { h } from '../render.js'

export function findLocale (document) {
  return document.documentElement.lang || 'en'
}

// Incomplete set obviously, but this is what Strava supports at time
// of writing.
const DOT_DECIMAL_LOCALES = new Set(['en', 'es-419', 'ko', 'ja', 'zh'])

export function parseLocalizedNumber (numberStr, locale) {
  const lang = locale.replace(/-.*$/, '')
  numberStr = numberStr.replaceAll(/[^0-9,.]/g, '')

  if (DOT_DECIMAL_LOCALES.has(locale) || DOT_DECIMAL_LOCALES.has(lang)) {
    return +numberStr.replaceAll(',', '')
  }

  return +numberStr.replaceAll('.', '').replaceAll(',', '.')
}

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
  async refreshGear (athleteId, locale) {
    console.info('Refreshing gear data.')
    const { bikes, shoes } = await fetchGear(athleteId, locale)
    const bikeComponents = {}

    for (const bike of bikes) {
      console.info('Refreshing components for bike', bike)
      bikeComponents[bike.id] = await fetchBikeComponents(bike.id, locale)
    }

    return {
      bikes,
      bikeComponents,
      shoes,

      lastFetchedAt: new Date()
    }
  }

}

async function fetchGear (athleteId, locale) {
  const baseURL = `https://www.strava.com/athletes/${athleteId}/gear/`

  const bikes = await fetchJSON(baseURL + 'bikes')
  const shoes = await fetchJSON(baseURL + 'shoes')

  const normalize = (list) => {
    return list
      .filter(it => it.active)
      .map(it => ({
        ...it,
        total_distance: parseLocalizedNumber(it.total_distance, locale)
      }))
  }

  return {
    bikes: normalize(bikes),
    shoes: normalize(shoes)
  }
}

async function fetchBikeComponents (gearId, locale) {
  const url = `https://www.strava.com/bikes/${gearId}`
  const doc = await fetchHTML(url)

  // Should have two elements:
  //   1. Bike details, not of interest
  //   2. Components
  const tables = doc.querySelectorAll('table')
  return parseBikeTable(tables[1], locale)
}

// TODO: this could be easily generalized.
function parseBikeTable (table, locale) {
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
      let val = node.innerText.trim()

      if (name === 'type') {
        const editLink = node.querySelector('a[href^="/components/"]')
        if (editLink !== null) {
          rowData.href = editLink.href
        }
      } else if (name === 'distance') {
        val = parseLocalizedNumber(val, locale)
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
  parseLocalizedNumber,
  locale: findLocale,

  dashboard: DashboardScraper,
  gear: GearScraper
}
