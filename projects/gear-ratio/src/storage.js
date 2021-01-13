const KEY = 'state'
const VERSION = 'v0'

// TODO: rename: PersistentState?
export class Storage {
  async persistState (state) {
    const persisted = await this.restoreState()
    await this.set(KEY, {
      [VERSION]: { ...persisted, ...state }
    })
  }

  async restoreState () {
    // NOTE: at some point it'd likely make sense to migrate between
    //   versions here
    const state = await this.get(KEY, {})
    return state[VERSION] || {}
  }

  async get (k, d) {
    const v = await browser.storage.local.get(k)
    return (typeof v === 'undefined') ? d : v[k]
  }

  async set (k, v) {
    await browser.storage.local.set({ [k]: v })
  }
}

export const storage = new Storage()
