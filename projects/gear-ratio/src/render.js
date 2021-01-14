function toDOMNode (thing) {
  if (thing instanceof Node) {
    return thing
  }

  if (typeof thing === 'function') {
    return toDOMNode(thing.call(this))
  }

  return document.createTextNode(thing.toString())
}

function createElement (tag, attrs, children) {
  let node = null

  if (typeof tag === 'string') {
    const ns = attrs.xmlns || 'http://www.w3.org/1999/xhtml'
    node = document.createElementNS(ns, tag)
  } else if (typeof tag === 'function') {
    node = toDOMNode(tag.call(h, attrs, children))
  } else if (typeof tag.render === 'function') {
    const props = {}
    const remainingAttrs = {}

    for (const k of (tag.props || [])) {
      if (!(k in attrs)) {
        console.warn(`[${tag}]: missing prop: ${k}`)
        continue
      }

      props[k] = attrs[k]
    }

    for (const k in attrs) {
      if (!(k in props)) {
        remainingAttrs[k] = attrs[k]
      }
    }

    attrs = remainingAttrs
    node = tag.render.call(props)
  } else {
    console.error('Unknown tag type', tag)
    node = toDOMNode(tag)
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
    .map(ch => toDOMNode(ch))
    .forEach(n => n && node.appendChild(n))

  return node
}

// shortcuts
export const h = createElement
