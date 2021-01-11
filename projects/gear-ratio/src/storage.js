const Key = {
  MIGRATION_VERSION: 'migration_version',
  BIKE_LINKS: 'v1.bike_links'
}

const MIGRATIONS = [
  async (s) => {
    await s.set('v1.bike_links', {})
  }
]

export class Storage {
  constructor () {
    this.testStorage = new Map()
  }

  async applyMigrations () {
    let currentVersion = await this.get(Key.MIGRATION_VERSION, 0)

    // Up to date, nothing to do
    if (MIGRATIONS.length === currentVersion) {
      return
    }

    if (MIGRATIONS.length < currentVersion) {
      console.error('Unexpected migration version, starting over!')
      currentVersion = 0
    }

    for (let i = currentVersion; i < MIGRATIONS.length; ++i) {
      console.info('Apply storage schema migration', i)
      await MIGRATIONS[i](this)
    }

    await this.set(Key.MIGRATION_VERSION, MIGRATIONS.length)
  }

  async get (k, d) {
    return k in this.testStorage ? this.testStorage[k] : d
    // TODO: json parse
    // const v = await browser.storage.sync.get(k)
    // return typeof v === 'undefined' ? d : v
  }

  async set (k, v) {
    // TODO: json stringify
    this.testStorage[k] = v
    // await browser.storage.sync.set(k, v)
  }

  async bikeLinks () {
    return await this.get(Key.BIKE_LINKS, {})
  }

  async setBikeLink (id, link) {
    const links = {
      [id]: link,
      ...await this.bikeLinks()
    }

    await this.set(Key.BIKE_LINKS, links)
  }
}

export const storage = new Storage()
