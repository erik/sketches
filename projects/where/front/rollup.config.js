import resolve from 'rollup-plugin-node-resolve';

export default {
    input: './src/app.js',
    output: {
        file: './build/app.js',
        format: 'cjs'
    },
    plugins: [
        resolve({
            module: true,
            jsnext: true,
            main: true,
            browser: true
        })
    ]
};
