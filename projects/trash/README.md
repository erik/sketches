# trash

Single file, zero dependency front end application framework.

It works, sorta.

I wanted to see how these libraries worked under the hood. You should
definitely not actually use this.

Works with JSX if you're into that.

``` javascript
/* @jsx h */

const ClickCounter = {
    props: ['title', 'count'],
    render (h) {
        return (
            <div>
                <h3> { this.count } </h3>
                { this.title }
            </div>
        );
    }
};

new Trash({
    el: '#app',
    data: {
        totalClicks: 0,
        clicks: {a: 0, b: 0, c: 0}
    },
    components: { ClickCounter },
    render: function (h) {
        const onClick = (title) => () => {
            this.totalClicks++; this.clicks[title]++;
        };

        const counters = Object.keys(this.clicks).map(title => {
            return <ClickCounter
                        title={ title }
                        count={ this.clicks[title] }
                        click={ onClick(title) } />;

        });

        return (
            <div>
                <h1>Total Clicks: { this.totalClicks }</h1>
                { counters }
            </div>
        );
    }
});

```
