// livewire -- no dependency JS reactivity

// i.e. not a class, not an array, etc.
function isObject(obj) {
  return obj?.__proto__ === {}.__proto__;
}

function wrapValue(v) {
  if (isObject(v)) return new Livewire(v, this);

  if (Array.isArray(v))
    return v.map((i) => (isObject(i) ? new Livewire(i, this) : i));

  return v;
}

export class Livewire {
  #state = null;
  #proxy = null;
  #parent = null;
  #queued = false;
  #computed = new Map();
  #observers = new Set();

  constructor(props, parent = null) {
    this.#parent = parent;
    this.#state = new Map();

    for (const [k, v] of Object.entries(props)) {
      if (k.startsWith("$")) {
        this.#computedKey(k, v);
      } else {
        this.#state[k] = wrapValue(v);
      }
    }

    this.#proxy = new Proxy(this.#state, {
      set: (target, key, value) => this.#set(target, key, value),
      get: (target, key) => this.#get(target, key),
    });

    return this.#proxy;
  }

  #set(target, key, value) {
    // Prevent setting computed properties
    if (this.#computed.has(key)) {
      console.warn(`Cannot set computed property: ${key}`);
      return false;
    }

    // TODO: doesn't work
    // Don't allow setting arbitrary properties
    if (!this.#state.hasOwnProperty(key)) {
      console.warn(`Cannot set unknown property: ${key}`);
      return false;
    }

    // Skip no-op updates
    const oldValue = target[key];
    if (oldValue === value) return true;

    Reflect.set(target, key, wrapValue(value));
    this.#deriveState();
    return true;
  }

  #get(target, key) {
    switch (key) {
      case "compute":
        return (key, fn) => this.#computedKey(key, fn);
      case "render":
        return (keys, fn) => this.#render(keys, fn);
      case "reactive":
        return ({ keys }, children) =>
          this.#render(keys, (state) =>
            createElement(
              "fragment",
              {},
              children.map((f) => f(state)),
            ),
          );
      case "reactiveEach":
        return ({ key }, children) =>
          this.#render([key], (state) =>
            createElement(
              "fragment",
              {},
              children.map((f) => state[key].map((v, i) => f(v, i, state))),
            ),
          );
      case "watch":
        return (keysOrFn, maybeFn) => this.#observer(keysOrFn, maybeFn);
      case "update":
        return (obj) =>
          Object.entries(obj).forEach(([k, v]) => this.#set(k, v));
      case "recompute":
        return () => this.#deriveState();
      default:
        return Reflect.get(target, key);
    }
  }

  #render(keys, fn) {
    let node = fn(this.#state);
    let anchor = null;
    let fragmentNodes = [];

    if (node instanceof DocumentFragment) {
      anchor = document.createComment("frag");
      node.appendChild(anchor);
      fragmentNodes = Array.from(node.childNodes);
    }

    const unwatch = this.#observer(keys, (state) => {
      const newNode = fn(state);

      // for fragments, replace all children
      if (anchor && anchor.parentNode) {
        const parent = anchor.parentNode;
        const nextSibling = anchor.nextSibling;

        // Remove old nodes
        fragmentNodes.forEach((n) => n.parentNode?.removeChild(n));

        // Insert new nodes
        if (newNode instanceof DocumentFragment) {
          fragmentNodes = Array.from(newNode.childNodes);
          parent.insertBefore(newNode, nextSibling);
          parent.insertBefore(anchor, nextSibling);
        } else {
          fragmentNodes = [newNode];
          parent.insertBefore(newNode, nextSibling);
          parent.insertBefore(anchor, nextSibling);
        }
      } else if (node && node.parentNode) {
        node.parentNode.replaceChild(newNode, node);
      } else if (node && !node.parentNode) {
        unwatch();
      }
      node = newNode;
    });

    return node;
  }

  #computedKey(key, fn) {
    this.#computed.set(key, fn);
    this.#state[key] = fn(this.#state);
    return this.#proxy;
  }

  #observer(...args) {
    let wrappedFn = args[0];

    // Watch specific keys
    if (typeof args[1] === "function") {
      const keys = new Set([args[0]].flat());
      const mapState = (s) => Object.entries(s).filter(([k]) => keys.has(k));

      let prev = JSON.stringify(mapState(this.#state));
      wrappedFn = (state) => {
        const curr = JSON.stringify(mapState(state));
        if (curr !== prev) {
          prev = curr;
          args[1].call(this, state);
        }
      };
    }

    this.#observers.add(wrappedFn);
    return () => this.#observers.delete(wrappedFn);
  }

  #deriveState() {
    // Child state could depend on parent state, so derive it first
    if (this.#parent) {
      this.#parent.recompute();
    }

    for (const [key, fn] of this.#computed) {
      this.#state[key] = fn(this.#state);
    }

    this.#observe();
  }

  #observe() {
    if (this.#queued) return;
    this.#queued = true;

    queueMicrotask(() => {
      this.#queued = false;
      for (const fn of this.#observers) {
        fn(this.#state);
      }
    });

    return this.#proxy;
  }
}

export function createFragment(_tag, children) {
  return _createElement("fragment", {}, ...children);
}

function _createElement(tag, attrs, ...children) {
  attrs = attrs || {};

  if (typeof tag === "function") {
    return tag(attrs, children);
  }

  let el =
    tag === "fragment"
      ? document.createDocumentFragment()
      : document.createElement(tag);

  for (const [k, v] of Object.entries(attrs)) {
    if (k === "$mount" && typeof v === "function") {
      queueMicrotask(() => v(el));
    } else if (k.startsWith("on") && typeof v === "function") {
      el.addEventListener(k.slice(2).toLowerCase(), v);
    } else if (k === "style" && typeof v === "object") {
      for (const p of Object.entries(v)) {
        el.style.setProperty(p[0], p[1]);
      }
    } else if (k === "className") {
      el.setAttribute("class", v);
    } else if (v === false || v === "") {
      el.removeAttribute(k);
    } else {
      el.setAttribute(k, v);
    }
  }

  for (const c of [children].flat(Infinity)) {
    if (c == null) continue;
    const v = typeof c === "function" ? c() : c;
    el.appendChild(v instanceof Node ? v : document.createTextNode(String(v)));
  }

  return el;
}

// magic curry sauce
export const createElement = new Proxy(_createElement, {
  get:
    (_target, prop, receiver) =>
    (attrs, ...children) =>
      receiver(prop, attrs, ...children),
});
