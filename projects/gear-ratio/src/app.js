export class App {
  constructor ({
    render,
    node,
    initialState,
    onEvent
  }) {
    this.isQueued = false
    this.isMounted = false
    this.renderError = false

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

  renderInternal () {
    const rendered = this.render(
      this.state,
      this.setState
    )

    this.node.replaceChildren(rendered)
    if (!this.isMounted) {
      this.isMounted = true
      this.onEvent('mounted')
    }
  }

  queueRender () {
    if (this.isQueued) return
    this.isQueued = true

    // Performing this action inside a immediately resolved promise
    // schedules the `.then` to be executed after all non-async work.
    //
    // Called a micro-task.
    Promise.resolve().then(async () => {
      try {
        this.renderInternal()
        this.renderError = false
      } catch (error) {
        console.exception('FAILED TO RENDER', error)

        if (!this.renderError) {
          this.renderError = true
          this.onEvent('error', { error })
        } else {
          console.error('WARNING: error in error handler! Dropping event')
        }
      }
      this.isQueued = false
    }).catch(err => {
      console.error('Something has gone very wrong...', err)
    })
  }
}
