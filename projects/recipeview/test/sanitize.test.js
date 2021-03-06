import assert from 'assert-diff';

import sanitize from '../src/js/sanitize.js';


describe('sanitize', () => {
    describe('image', () => {
        it('handles undefined and null', () => {
            assert.equal(sanitize.image(null), null);
            assert.equal(sanitize.image(undefined), null);
        });

        it('handle simple strings', () => {
            assert.equal(sanitize.image('http://image.url'), 'http://image.url');
        });

        it('handles lists', () => {
            assert.equal(sanitize.image(['foo', 'bar']), 'foo');
        });

        it('handles microdata lists', () => {
            assert.equal(sanitize.image({'@list': ['foobar']}), 'foobar');
            assert.equal(sanitize.image({'@list': [{url: 'foobar'}]}), 'foobar');
        });
    });

    describe('author', () => {
        it('handles undefined and null', () => {
            assert.equal(sanitize.author(null), null);
            assert.equal(sanitize.author(undefined), null);
        });

        it('works with lists', () => {
            assert.equal(sanitize.author(['1', '2', '3']), '1');
        });

        it('handles empty names', () => {
            assert.equal(sanitize.author({name: ''}), null);
        });
    });

    describe('time', () => {
        it('returns null on bad data', () => {
            [null, 'not a duration', 'P7D4', 'P'].forEach(duration => {
                assert.equal(sanitize.time(duration), null);
            });
        });

        it('handles common formats', () => {
            assert.equal(sanitize.time('P20Y300M34DT12H34M56.0S'), '12 hr 34 min');
            assert.equal(sanitize.time('PT12H34M'), '12 hr 34 min');
            assert.equal(sanitize.time('PT12H'), '12 hr');
            assert.equal(sanitize.time('PT12M'), '12 min');
        });

        it('ignores hrs=0, min=0', () => {
            assert.equal(sanitize.time('PT0H12M'), '12 min');
            assert.equal(sanitize.time('PT12H0M'), '12 hr');
            assert.equal(sanitize.time('PT0H0M'), null);
        });
    });

    describe('ingredient', () => {
        it('does not mangle unknown formats', () => {
            ['abcd', 'bananas, 32 of them'].forEach(i => {
                assert.deepEqual(sanitize.ingredient(i), {ingredient: i});
            });
        });

        it('handles fractions', () => {
            assert.deepEqual(sanitize.ingredient('½ tsp potato'), {
                quantity: '½',
                unit: 'tsp',
                ingredient: 'potato'
            });

            assert.deepEqual(sanitize.ingredient('32 ½ cloves potato'), {
                quantity: '32 ½',
                unit: 'cloves',
                ingredient: 'potato'
            });

            assert.deepEqual(sanitize.ingredient('8⁄2 grams potato'), {
                quantity: '8⁄2',
                unit: 'grams',
                ingredient: 'potato'
            });
        });

        it('handles missing units', () => {
            assert.deepEqual(sanitize.ingredient('52 grapes'), {
                quantity: '52',
                unit: null,
                ingredient: 'grapes'
            });
        });
    });

    describe('string', () => {
        it('strips out leftover HTML tags', () => {
            assert.equal(sanitize.string('foo <a href="foobar.com">bar</a> baz'), 'foo bar baz');
            assert.equal(sanitize.string('foo <a href="unclosed string>bar</a>'), 'foo bar');
        });

        it('replaces fractions', () => {
            assert.equal(sanitize.string('1/2 1/3 1/4 55/99'), '½ ⅓ ¼ 55⁄99');
        });

        it('replaces encoded html entities', () => {
            assert.equal(sanitize.string('foo&amp;bar'), 'foo&bar');
        });

        it('works on double encoded HTML', () => {
            assert.equal(
                sanitize.string('&lt;span&gt;foo&amp;bar&lt;/span&gt;'),
                'foo&bar');
        });

        it('is not vulnerable to XSS', () => {
            const input = `<img src="x" onerror="alert('xss'); window.FAILED = true" />`;
            assert.equal(sanitize.string(input), '');
            assert.equal(typeof window.FAILED, 'undefined');
        });
    });

    describe('common', () => {
        it('maps over arrays', () => {
            assert.deepEqual(sanitize.common(['&amp;', 'foo', '1']), ['&', 'foo', '1']);
        });

        it('handles strings', () => {
            assert.equal(sanitize.common('1/2'), '½');
        });

        it('leaves other things untouched', () => {
            assert.deepEqual(sanitize.common({a: 2}), {a: 2});
        });
    });

    describe('yield', () => {
        it('handles strings', () => {
            assert.equal(sanitize.yield(1), '1');
        });
    });

    describe('instructions', () => {
        it('converts text blocks with numbers to lists', () => {
            const instr = sanitize.instructions('1. stir\n2. bake\n3. serve');

            assert.equal(instr.text, null);
            assert.deepEqual(instr.list, ['stir', 'bake', 'serve']);
        });

        it('converts text blocks with line breaks to lists', () => {
            const instr = sanitize.instructions('stir\nbake\nserve');

            assert.equal(instr.text, null);
            assert.deepEqual(instr.list, ['stir', 'bake', 'serve']);
        });

        it('converts text blocks with probable missing newlines to lists', () => {
            const instr = sanitize.instructions('stir.now bake. and then.serve');

            assert.equal(instr.text, null);
            assert.deepEqual(instr.list, ['stir.', 'now bake. and then.', 'serve']);
        });

        it('leaves regular text blocks alone', () => {
            const text = 'heat oven to xyz. cook for whatever. then do this.';
            const instr = sanitize.instructions(text);

            assert.equal(instr.list, null);
            assert.equal(instr.text, text);
        });
    });

    describe('stripTags', () => {
        it('strips simple cases', () => {
            const input = `foo <a src='asdf' href="bar baz quux">bar</a> baz`;
            assert.equal(sanitize.stripTags(input), 'foo bar baz');
        });

        it('strips nested tags', () => {
            const input = `foo <a src='asdf' href="bar baz quux">bar <span class="foo">baz</span></a>`;
            assert.equal(sanitize.stripTags(input), 'foo bar baz');
        });

        it('strips bad html', () => {
            const input = `fizz<a href='"></a>buzz`;
            assert.equal(sanitize.stripTags(input), 'fizzbuzz');
        });
    });
});
