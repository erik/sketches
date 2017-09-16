class Observer {
    constructor (inst) {
        this.instance = inst;
    }

    observe(key, value) {
        Object.defineProperty(this.instance, key, {
            get: () => {
                return this.instance.$data[key];
            },

            set: (value) => {
                this.instance.$data[key] = value;
                this.instance.$notify(key);
            }
        });
    }
}


class Trash {
    constructor (options) {
        this.$data = options.data || {};
        this.$hooks = options.hooks || {};
        this.$renderer = options.render;
        this.$components = options.components || {};
        this.$dirty = true;
        this.$observer = new Observer(this);

        for (let k in this.$data) {
            let v = this.$data[k];
            this.$observer.observe(k, v);
        }

        if (options.el) {
            this.$mount(options.el);
        }
    }

    $mount (element) {
        console.log('mounting to el', element);
        this.$el = document.querySelector(element);

        this.$hooks.mounted.call(this);
        this.$render();
    }

    $notify (key) {
        if (this.$dirty) return;
        this.$dirty = true;

        // microtask
        Promise.resolve().then(() => { this.$render(false); });
    }

    $render (initial) {
        if (!initial && !this.$dirty) return;

        const domify = (item) => {
            if (item instanceof Node) { return item; }
            else if (typeof item === 'function') {
                return domify(item.call(this));
            } else if (typeof item === 'string') {
                return document.createTextNode(item);
            } else {
                return document.createTextNode(item.toString());
            }
        };

        const createElement = (tag, attrs, children) => {

            // Special handling for components.
            if (tag in this.$components) {
                const component = this.$components[tag];
                let props = {};

                component.props.forEach(k => {
                    if (!(k in attrs)) {
                        console.warn(`[${tag}]: missing prop: ${k}`);
                    }

                    props[k] = attrs[k];
                });

                const result = component.render.call(props, createElement);
                return domify(result);
            }

            const el = document.createElement(tag);

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
                el.appendChild(domify(child));
            });

            return el;
        };

        const newElem = this.$renderer(createElement);
        this.$el.replaceChild(newElem, this.$el.firstChild);
        this.$dirty = false;
    }
};
