// livewire -- no dependency JS reactivity

// i.e. not a class, not an array, etc.
function isObject(obj: any) {
  // @ts-ignore -- TODO: why
  return obj?.__proto__ === {}.__proto__;
}

function wrapValue(v: any, parent?: Livewire<any>) {
  if (isObject(v)) {
    return new Livewire(v, parent);
  } else if (Array.isArray(v)) {
    // TODO: does this matter?
    return v.map((i) => (isObject(i) ? new Livewire(i, parent) : i));
  }

  return v;
}

export class Livewire<P extends Record<string, any>> {
  #state = {} as P;
  #parent?: Livewire<any> = null;
  $: P;
  #queued = false;
  #computed = new Map();
  #observers = new Set();

  constructor(props: P, parent?: Livewire<any>) {
    this.#parent = parent;

    for (const [k, v] of Object.entries(props)) {
      if (k.startsWith("$")) {
        this.compute(k, v);
      } else {
        (this.#state as Record<string, any>)[k] = wrapValue(v, this);
      }
    }

    this.$ = new Proxy(this.#state, {
      set: (target, key, value) => this.#set(target, key, value),
      get: (target, key) => Reflect.get(target, key),
    }) as P;
  }

  #set(target, key, value) {
    // Prevent setting computed properties
    if (this.#computed.has(key)) {
      throw Error(`Cannot set computed property: ${key}`);
    }

    // TODO: doesn't work
    // Don't allow setting arbitrary properties
    if (!(key in this.#state)) {
      throw Error(`tried to set unknown prop: ${key}`);
    }

    // Skip no-op updates
    const oldValue = target[key];
    if (oldValue === value) return true;

    Reflect.set(target, key, wrapValue(value, this));
    this.tick();
    return true;
  }

  reactive = ({ keys }, children: ((state: P) => any)[]) => {
    return this.render(keys, (state: P) =>
      createElement(
        "fragment",
        {},
        children.map((fn) => fn(state)),
      ),
    );
  };

  reactiveEach = (
    { key },
    children: ((value: any, index: number, state: P) => any)[],
  ) => {
    return this.render([key], (state: P) =>
      createElement(
        "fragment",
        {},
        children.map((fn) =>
          state[key].map((value: any, index: number) =>
            fn(value, index, state),
          ),
        ),
      ),
    );
  };

  render(keys: string | string[], fn: (state: P) => any) {
    console.log("render called with keys:", keys);
    console.log("initial state:", this.#state);
    let node = fn(this.#state);
    let anchor = null;
    let fragmentNodes = [];

    if (node instanceof DocumentFragment) {
      anchor = document.createComment("frag");
      node.appendChild(anchor);
      fragmentNodes = Array.from(node.childNodes);
    }

    const unwatch = this.watch(keys, (state: P) => {
      console.log("watch triggered with state:", state);
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

  compute(key: string, fn: (state: P) => any) {
    this.#computed.set(key, fn);
    (this.#state as Record<string, any>)[key] = fn(this.#state);
    return this;
  }

  watch(
    maybeFn: string | string[] | ((state: P) => any),
    fn: ((state: P) => any) | undefined,
  ) {
    let wrappedFn: (state: P) => any;

    if (typeof maybeFn === "function") {
      wrappedFn = maybeFn;
    } else {
      // Watch specific keys
      const keys = new Set([maybeFn].flat());
      const filterState = (s: P) =>
        Object.entries(s).filter(([k]) => keys.has(k));

      let prev = JSON.stringify(filterState(this.#state));
      wrappedFn = (state: P) => {
        const curr = JSON.stringify(filterState(state));
        if (curr !== prev) {
          prev = curr;
          fn.call(this, state);
        }
      };
    }

    this.#observers.add(wrappedFn);
    return () => this.#observers.delete(wrappedFn);
  }

  protected tick() {
    // Child state could depend on parent state, so derive it first
    this.#parent?.tick();

    for (const [key, fn] of this.#computed) {
      (this.#state as Record<string, any>)[key] = fn(this.#state);
    }

    this.#triggerWatchers();
  }

  #triggerWatchers() {
    if (this.#queued) return;
    this.#queued = true;

    queueMicrotask(() => {
      this.#queued = false;
      for (const fn of this.#observers) {
        // TODO: fix typing
        // @ts-ignore
        fn(this.#state);
      }
    });
  }
}

export function createFragment(_tag, children) {
  return _createElement("fragment", {}, ...children);
}

export function htmlTemplate(s: string) {
  return _createElement("template", { innerHTML: s }).content;
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
    } else if (k === "innerHTML") {
      el.innerHTML = v;
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
