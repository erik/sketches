// livewire -- no dependency JS reactivity

export type Props = Record<string, any>;
export type StateFn<P, C> = (state: P & C) => any;
export type StateAction<P, C> = (state: P & C, ...args: any[]) => Partial<P>;
export type StateActions<P, C> = Record<string, StateAction<P, C>>;
export type RenderFn<P, C> = (state: P & C) => Child;

export type ElementAttrs = Record<string, any>;
export type Child =
  | Node
  | string
  | number
  | boolean
  | null
  | undefined
  | (() => Child);
export type Children = Child[];

export class Livewire<P extends Props, C extends Props = {}> {
  #state = {} as P & C;
  $: P & C;
  #computed = new Map<keyof C, StateFn<P, C>>();
  #observers = new Set<StateFn<P, C>>();

  #queued = false;

  constructor(props: P & Partial<C>) {
    this.#state = {} as P & C;

    for (const [k, v] of Object.entries(props)) {
      if (k.startsWith("$")) {
        this.compute(k, v);
      } else {
        this.#state[k as keyof P] = v;
      }
    }

    this.$ = new Proxy<P & C>(this.#state, {
      set: (target, key: string, value: any) => this.#set(key, value),
      get: (target, key: string) => Reflect.get(target, key),
    });
  }

  #set(key: string, value: any) {
    // Prevent setting computed properties
    if (this.#computed.has(key)) {
      throw Error(`Cannot set computed property: ${key}`);
    }

    // Don't allow setting arbitrary properties
    if (!(key in this.#state)) {
      throw Error(`tried to set unknown prop: ${key}`);
    }

    // Skip no-op updates
    if (this.#state[key] === value) return true;

    Reflect.set(this.#state, key, value);
    this.#queueTick();
    return true;
  }

  update<K extends keyof P>(key: K & string, fn: (value: P[K]) => any) {
    const val = fn(this.$[key]);
    this.#set(key, val);
  }

  reactive = (
    { keys }: { keys: string | string[] },
    ...children: (RenderFn<P, C> | Child)[]
  ) => {
    return this.render(keys, (state) =>
      createFragment(
        {},
        ...children.map((f) => (typeof f === "function" ? f(state) : f)),
      ),
    );
  };

  reactiveEach = (
    { key }: { key: string },
    ...children: ((value: any, index: number, state: P & C) => Child)[]
  ) => {
    return this.render([key], (state: P & C) =>
      createFragment(
        {},
        ...children.map((fn) =>
          state[key].map((value: any, index: number) =>
            fn(value, index, state),
          ),
        ),
      ),
    );
  };

  render(keys: string | string[], fn: RenderFn<P, C>): Child {
    const fragment = document.createDocumentFragment();
    const anchor = document.createComment(`render(${keys.toString()})`);
    fragment.appendChild(anchor);

    let prev: Node[] = [];

    const _rerender = (state: P & C) => {
      prev.forEach((n) => n.parentNode?.removeChild(n));

      const node = toDOMNode(fn(state));
      if (node != null) {
        prev =
          node instanceof DocumentFragment
            ? Array.from(node.childNodes)
            : [node];
        anchor.parentNode?.insertBefore(node, anchor.nextSibling);
      } else {
        prev = [];
      }
    };

    _rerender(this.#state);

    const unwatch = this.watch(keys, (state: P & C) => {
      // We've been removed from the DOM, clean up watcher
      if (!anchor.isConnected) {
        console.log("anchor disconnect", keys);
        return unwatch();
      }
      _rerender(state);
    });

    return fragment;
  }

  compute(key: keyof C, fn: StateFn<P, C>): this {
    this.#computed.set(key, fn);
    this.#state[key] = fn(this.#state);
    return this;
  }

  watch(
    maybeFn: string | string[] | StateFn<P, C>,
    fn?: StateFn<P, C>,
  ): () => void {
    let wrappedFn: StateFn<P, C>;

    if (typeof maybeFn === "function") {
      wrappedFn = maybeFn;
    } else {
      const keys = new Set([maybeFn].flat(Infinity));
      const filterState = (s: P & C) =>
        Object.entries(s).filter(([k]) => keys.has(k));

      let prev: string;
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

  #queueTick() {
    if (this.#queued) return;
    this.#queued = true;

    queueMicrotask(() => {
      this.#queued = false;

      for (const [key, fn] of this.#computed) {
        this.#state[key] = fn(this.#state);
      }

      for (const fn of this.#observers) {
        fn(this.#state);
      }
    });
  }
}

export function createFragment(
  attrs: ElementAttrs,
  ...children: Children
): DocumentFragment {
  return createElement("fragment", attrs, ...children) as DocumentFragment;
}

export function htmlTemplate(s: TemplateStringsArray): DocumentFragment {
  const el = document.createElement("template");
  // TODO: fixme
  el.innerHTML = s.toString();
  return el.content;
}

export function createElement(
  tag: string | ((attrs?: ElementAttrs, ...children: Children) => Element),
  attrs?: ElementAttrs,
  ...children: Children
): Element | DocumentFragment {
  attrs = attrs || {};

  if (typeof tag === "function") {
    return tag(attrs, ...children);
  }

  let el =
    tag === "fragment"
      ? document.createDocumentFragment()
      : document.createElement(tag);

  for (const [k, v] of Object.entries(attrs)) {
    if (k === "$mount" && typeof v === "function") {
      queueMicrotask(() => v(el));
    } else if (k.startsWith("on") && typeof v === "function") {
      el.addEventListener(k.slice(2).toLowerCase(), v as (_: Event) => void);
    } else if (el instanceof HTMLElement) {
      if (k === "style" && typeof v === "object") {
        for (const p of Object.entries(v)) {
          el.style.setProperty(p[0], String(p[1]));
        }
      } else if (k === "className") {
        el.setAttribute("class", String(v));
      } else if (k === "innerHTML") {
        el.innerHTML = String(v);
      } else if (v === false || v === "" || v == null) {
        el.removeAttribute(k);
      } else {
        el.setAttribute(k, String(v));
      }
    }
  }

  for (const c of [children].flat(Infinity)) {
    const node = toDOMNode(c as Child);
    if (node != null) {
      el.appendChild(node);
    }
  }

  return el;
}

function toDOMNode(val: Child): Node | null {
  if (val instanceof Node) return val;
  if (typeof val === "function") return toDOMNode(val());

  if (
    val == null ||
    val === false ||
    (Array.isArray(val) && val.length === 0)
  ) {
    return null;
  }

  return document.createTextNode(String(val));
}
