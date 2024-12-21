const path = require('path');

module.exports = {
  mode: 'development',
  target: 'web',
  entry: '/workspace/elm-project',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'index.js'
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
            optimize: true
          }
        }
      }
    ]
  },
  devServer: {
    contentBase: path.join(__dirname, '/workspace/elm-project/index.html'),
    port: 8080
  }
};
