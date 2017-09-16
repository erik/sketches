const toDomNode = (item) => {
    if (item instanceof Node) {
        return item;
    }
    // Basic thunk support
    else if (typeof item === 'function') {
        return toDomNode(item.call(this));
    }

    return document.createTextNode(item.toString());
};


function createElement (tag, attrs, children) {
    let el;

    // Special handling for components.
    if (tag in this.$components) {
        const component = this.$components[tag];
        let props = {};

        component.props.forEach(k => {
            if (!(k in attrs)) {
                console.warn(`[${tag}]: missing prop: ${k}`);
                return;
            }

            props[k] = attrs[k];
            delete attrs[k];
        });

        const rendered = component.render.call(props, createElement.bind(this));
        el = toDomNode(rendered);
    } else {
        el = document.createElement(tag);
    }

    for (const k in attrs) {
        const v = attrs[k];

        if (typeof v === 'function') {
            el.addEventListener(k, v.bind(this));
        } else if (k === 'style') {
            for (const prop in v) {
                el.style.setProperty(prop, v[prop]);
            }
        } else {
            el.setAttribute(k, v);
        }
    }

    if (!Array.isArray(children)) {
        children = [children];
    }

    children.forEach(child => {
        el.appendChild(toDomNode(child));
    });

    return el;
}


class Trash {
    constructor (options) {
        this.$data = options.data || {};
        this.$renderFn = options.render;
        this.$components = options.components || {};
        this.$renderQueued = false;

        for (let k in this.$data) {
            Object.defineProperty(this, k, {
                get: () => this.$data[k],
                set: (value) => {
                    this.$queueRender();
                    this.$data[k] = value;
                }
            });
        }

        options.el && this.$mount(options.el);
    }

    $mount (element) {
        this.$el = document.querySelector(element);
        this.$queueRender();
    }

    $queueRender () {
        if (this.$renderQueued) return;
        this.$renderQueued = true;

        // enqueue microtask
        Promise.resolve().then(() => {
            const newElem = this.$renderFn(createElement.bind(this));
            this.$el.replaceChild(newElem, this.$el.firstChild);

            this.$renderQueued = false;
        });
    }
};
