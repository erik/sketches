// livewire -- no dependency JS reactivity

export type Props = Record<string, any>;
export type StateFn<P, C> = (state: P & C) => any;
export type StateAction<P, C> = (state: P & C, ...args: any[]) => Partial<P>;
export type StateActions<P, C> = Record<string, StateAction<P, C>>;

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
    this.#queueTick();
    return true;
  }

  update<K extends keyof P>(key: K, fn: (value: P[K]) => any) {
    this.$[key] = fn(this.$[key]);
  }

  reactiveIf = (
    { key }: { key: string },
    trueBranch: StateFn<P, C>,
    falseBranch: StateFn<P, C> = () => undefined,
  ) =>
    this.render([key], (state) =>
      createFragment({}, !!state[key] ? trueBranch(state) : falseBranch(state)),
    );

  reactive = (
    { keys }: { keys: string | string[] },
    ...children: StateFn<P, C>[]
  ) => {
    return this.render(keys, (state) =>
      createFragment({}, ...children.map((fn) => fn(state))),
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

  render(keys: string | string[], fn: StateFn<P, C>): Node {
    let prevNode = fn(this.#state);
    let fragAnchor = null;
    let fragment: Array<Node> = [];

    if (prevNode instanceof DocumentFragment) {
      fragAnchor = document.createComment("frag");
      prevNode.appendChild(fragAnchor);
      fragment = Array.from(prevNode.childNodes);
    }

    const unwatch = this.watch(keys, (state: P & C) => {
      const nextNode = fn(state);

      // for fragments, replace all children
      if (fragAnchor && fragAnchor.parentNode) {
        const parent = fragAnchor.parentNode;
        const sibling = fragAnchor.nextSibling;

        // Remove old nodes
        fragment.forEach((n) => n.parentNode?.removeChild(n));
        fragment =
          nextNode instanceof DocumentFragment
            ? Array.from(nextNode.childNodes)
            : [nextNode];

        parent.insertBefore(nextNode, sibling);
        parent.insertBefore(fragAnchor, sibling);
      } else if (prevNode && prevNode.parentNode) {
        prevNode.parentNode.replaceChild(nextNode, prevNode);
      } else if (prevNode && !prevNode.parentNode) {
        unwatch();
      }
      prevNode = nextNode;
    });

    return prevNode;
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
    const v = typeof c === "function" ? c() : c;
    if (v == null || v === false || (Array.isArray(v) && v.length === 0)) {
      continue;
    }

    el.appendChild(v instanceof Node ? v : document.createTextNode(String(v)));
  }

  return el;
}
