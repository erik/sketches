class Observer {
    constructor (inst, data) {
        this.instance = inst;
        this.observeTree(data);
    }

    observeTree (map) {
        for (let k in map) {
            let v = map[k];
            this.observe(k, v);
        }
    }

    observe(key, value) {
        Object.defineProperty(this.instance, key, {
            get: () => { return this.instance.$data[key]; },

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
        this.$dirty = true;

        this.$observer = new Observer(this, this.$data);

        if (options.el) {
            this.$mount(options.el);
        }
    }

    $mount (element) {
        console.log('mounting to el', element);
        this.$el = document.querySelector(element);

        this.$hooks.mounted.bind(this)();
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
                window.alert('i dont know what this is');
                return null;
            }
        };

        const createElement = (tag, attrs, children) => {
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
