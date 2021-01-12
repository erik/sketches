const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')

const BUILD_DIR = path.resolve(__dirname, 'build')

module.exports = {
  entry: {
    'content_script/dashboard': './src/content_script/dashboard.js',
    'content_script/gear': './src/content_script/gear.js'
  },

  output: {
    path: BUILD_DIR,
    filename: '[name].js'
  },

  // TODO: parameterize
  mode: 'development',
  optimization: { minimize: false },

  module: {
  },

  // TODO: Copy from web-extension-polyfill
  plugins: [
    new CopyWebpackPlugin({
      patterns: [
        { from: './addon/', to: BUILD_DIR }
      ]
    })
  ]
}
