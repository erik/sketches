class Observer {
    constructor (inst) {
        this.dependencies = new Set();
        this.instance = inst;
    }

    clearDependencies () { this.dependencies = new Set(); }

    observe(key, value) {
        console.log('observing', key, 'initially', value);

        Object.defineProperty(this.instance, key, {
            get: () => {
                this.dependencies.add(key);
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
        this.$observer = new Observer(this);
        this.$data = options.data || {};
        this.$hooks = options.hooks || {};
        this.$renderer = options.render;
        this.$dirty = true;

        for (let k in this.$data) {
            this.$observer.observe(k, options.data[k]);
        }

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

        console.log('key modified', key, 'queuing change');
        this.$dirty = true;

        // microtask
        Promise.resolve().then(() => {
            console.log('repaint');
            this.$render(false);
        });
    }

    $render (initial) {
        if (!initial && !this.$dirty) return;

        let createElement = (tag, props, children) => {
            let el = document.createElement(tag);
            children.forEach(child => {
                switch (typeof child) {
                case 'function':
                    el.appendChild(child.bind(this)());
                    break;
                case 'string':
                    el.appendChild(document.createTextNode(child));
                    break;
                default:
                    // assuming this is a node but that's not a safe assumption
                    el.appendChild(child);
                }
            });

            return el;
        };

        this.$observer.clearDependencies();

        let newElem = this.$renderer(createElement);

        this.$el.replaceChild(newElem, this.$el.childNodes[0]);
        this.$dirty = false;
    }
};


window.onload = () => {
    let t = new Trash({
        el: '#app',
        data: {x: 1, y: true, date: null},

        hooks: {
            mounted: function () {
                console.log('i was mounted', this);
            },
            destroyed: function () { console.log('i was destroyed'); }
        },
        render: function(h) {
            return h('div', {}, [
                h('h1', {}, [
                    'straight trash',
                    h('small', {}, [`updated: ${this.date}`])
                ]),
                `x = ${this.x}`,
                () => { return h('div', {}, [this.y ? 'y is true' : 'y is false']); },

                h('ul', {}, Array.from(Array(100)).map((_, i) => {
                    return h('ol', {}, [`${i+this.x}`]);
                }))
            ]);
        }
    });


    setInterval(() => {
        console.log('poking things');
        t.date = new Date();
        t.x ++;
        t.y = !t.y;
    }, 400);
}
