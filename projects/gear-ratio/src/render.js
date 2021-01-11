function createNode (tag, props, children) {
  const ns = props.xmlns || 'http://www.w3.org/1999/xhtml'
  const node = document.createElementNS(ns, tag)

  for (const key in props) {
    const val = props[key]

    switch (key) {
      case 'class':
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
export const h = createNode
