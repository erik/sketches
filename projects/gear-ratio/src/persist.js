const KEY = 'state'
const VERSION = 'v0'

export class PersistentState {
  async persist (state) {
    const persisted = await this.restore()
    await this.set(KEY, {
      [VERSION]: { ...persisted, ...state }
    })
  }

  async restore () {
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

export const persistentState = new PersistentState()
