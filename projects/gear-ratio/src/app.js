export class App {
  constructor ({
    render,
    node,
    initialState,
    onEvent
  }) {
    this.isQueued = false
    this.isMounted = false

    this.render = render
    this.state = initialState

    this.eventHandlers = onEvent || {}
    node && this.mount(node)
  }

  mount (node) {
    this.node = node
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
    }).catch(error => {
      this.onEvent('error', { error })
    })
  }
}
