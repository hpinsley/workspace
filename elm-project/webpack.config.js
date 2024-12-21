const path = require('path');

module.exports = {
  mode: 'development',
  entry: '/workspace/elm-project/index.html',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.html$/i,
        loader: "html-loader",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
          },
          options: {
            files: ['Main.elm'],
            cwd: '.',
            optimize: false,
            debug: true
          }
        }
      }
    ]
  },
  devServer: {
    contentBase: path.join(__dirname, '/workspace/elm-project/index.html'),
    port: 8080
  }

  // devServer: {
  //   contentBase: './dist'
  // }
};
