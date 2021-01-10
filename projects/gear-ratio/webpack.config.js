const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')

const BUILD_DIR = path.resolve(__dirname, 'build')

module.exports = {
  entry: {
    content_script: './src/content_script.js'
  },

  output: {
    path: BUILD_DIR,
    filename: '[name].js'
  },

  module: {
  },

  plugins: [
    new CopyWebpackPlugin({
      patterns: [
        { from: './addon/', to: BUILD_DIR }
      ]
    })
  ]
}
