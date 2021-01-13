function createNode (tag, attrs, children) {
  let node

  if (typeof tag === 'string') {
    const ns = attrs.xmlns || 'http://www.w3.org/1999/xhtml'
    node = document.createElementNS(ns, tag)
  } else if (typeof tag === 'function') {
    node = tag.call(h, attrs, children)
  } else if (typeof tag.render === 'function') {
    const props = {}
    // collect everything left in attrs that isn't consumed by props
    const nonprops = {}

    for (const k of (tag.props || [])) {
      if (!(k in attrs)) {
        console.warn(`[${tag}]: missing prop: ${k}`)
        continue
      }

      props[k] = attrs[k]
    }

    for (const k in attrs) {
      if (!(k in props)) {
        nonprops[k] = attrs[k]
      }
    }

    attrs = nonprops
    node = tag.render.call(props)
  } else {
    console.error('Unknown tag type', tag)
  }

  for (const key in attrs) {
    const val = attrs[key]

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
