// livewire -- no dependency JS reactivity

type Props = Record<string, any>;
type StateFn<P, C> = (state: P & C) => any;
type StateAction<P, C> = (state: P & C, ...args: any[]) => Partial<P>;
type StateActions<P, C> = Record<string, StateAction<P, C>>;

type LivewireOptions<A> = {
  actions?: A;
};

export class Livewire<
  P extends Props,
  C extends Props = {},
  A extends StateActions<P, C> = {},
> {
  #state = {} as P & C;
  #actions = {} as A;
  $: P & C;
  #computed = new Map<keyof C, StateFn<P, C>>();
  #observers = new Set<StateFn<P, C>>();

  #queued = false;

  constructor(props: P & Partial<C>, options: LivewireOptions<A> = {}) {
    this.#state = {} as P & C;
    options.actions && (this.#actions = options.actions);

    for (const [k, v] of Object.entries(props)) {
      if (k.startsWith("$")) {
        this.compute(k, v);
      } else {
        this.#state[k as keyof P] = v;
      }
    }

    this.$ = new Proxy<P & C>(this.#state, {
      set: (target, key: string, value: any) => this.#set(target, key, value),
      get: (target, key: string) => Reflect.get(target, key),
    });
  }

  #set(target: P & C, key: string, value: any) {
    // Prevent setting computed properties
    if (this.#computed.has(key)) {
      throw Error(`Cannot set computed property: ${key}`);
    }

    // Don't allow setting arbitrary properties
    if (!(key in this.#state)) {
      throw Error(`tried to set unknown prop: ${key}`);
    }

    // Skip no-op updates
    const oldValue = target[key];
    if (oldValue === value) return true;

    Reflect.set(target, key, value);
    this.tick();
    return true;
  }

  reduce(fn: StateAction<P, C>, ...args: any[]) {
    this.#state = {
      ...this.#state,
      ...fn(this.#state, ...args),
    };
    this.tick();
  }

  dispatch(action: keyof A, ...args: any[]) {
    this.reduce(this.#actions[action], ...args);
  }

  reactive = ({ keys }, children: StateFn<P, C>[]) => {
    return this.render(keys, (state) =>
      createFragment(
        {},
        children.map((fn) => fn(state)),
      ),
    );
  };

  reactiveEach = (
    { key },
    children: ((value: any, index: number, state: P & C) => any)[],
  ) => {
    return this.render([key], (state: P & C) =>
      createFragment(
        {},
        children.map((fn) =>
          state[key].map((value, index) => fn(value, index, state)),
        ),
      ),
    );
  };

  render(keys: string | string[], fn: StateFn<P, C>) {
    let node = fn(this.#state);
    let anchor = null;
    let fragmentNodes = [];

    if (node instanceof DocumentFragment) {
      anchor = document.createComment("frag");
      node.appendChild(anchor);
      fragmentNodes = Array.from(node.childNodes);
    }

    const unwatch = this.watch(keys, (state: P & C) => {
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

  compute(key: keyof C, fn: StateFn<P, C>) {
    this.#computed.set(key, fn);
    this.#state[key as keyof C] = fn(this.#state);
    return this;
  }

  watch(maybeFn: string | string[] | StateFn<P, C>, fn?: StateFn<P, C>) {
    let wrappedFn: StateFn<P, C>;

    if (typeof maybeFn === "function") {
      wrappedFn = maybeFn;
    } else {
      const keys = new Set([maybeFn].flat(Infinity));
      const filterState = (s: P & C) =>
        Object.entries(s).filter(([k]) => keys.has(k));

      let prev = null;
      wrappedFn = (state: P & C) => {
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
    for (const [key, fn] of this.#computed) {
      this.#state[key] = fn(this.#state);
    }

    this.#triggerWatchers();
  }

  #triggerWatchers() {
    if (this.#queued) return;
    this.#queued = true;

    queueMicrotask(() => {
      this.#queued = false;
      for (const fn of this.#observers) {
        fn(this.#state);
      }
    });
  }
}

export function createFragment(attrs, children) {
  return _createElement("fragment", attrs, children);
}

export function htmlTemplate(s: TemplateStringsArray): Element {
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
    } else if (v === false || v === "" || v == null) {
      el.removeAttribute(k);
    } else {
      el.setAttribute(k, v);
    }
  }

  for (const c of [children].flat(Infinity)) {
    const v = typeof c === "function" ? c() : c;
    if (v == null || v === false || (Array.isArray(v) && v.length === 0)) {
      continue;
    }

    el.appendChild(v instanceof Node ? v : document.createTextNode(String(v)));
  }

  return el;
}

type CreateElementCurry = {
  [key: string]: (attrs: Record<string, any>, ...children: any[]) => any;
} & {
  (tag: string, attrs?: Record<string, any>, ...children: any[]): any;
};

// magic curry sauce
export const createElement = new Proxy(_createElement, {
  get:
    (_, prop) =>
    (attrs: Record<string, any>, ...children: any[]) =>
      _createElement(prop, attrs, ...children),
}) as CreateElementCurry;
