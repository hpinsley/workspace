const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  mode: 'development',
  entry: {
    app: [
      './src/index.js'
    ]
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'index.js'
  },
  // plugins: [new HtmlWebpackPlugin({
  //   template: "template.html",
  // })],
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
            // files: ['Main.elm'],
            cwd: '.',
            optimize: false,
            debug: true
          }
        }
      }
    ]
  },
  devServer: {
    // contentBase: '/workspace/elm-project',
    host: '0.0.0.0',
    port: 8080
  }

  // devServer: {
  //   contentBase: './dist'
  // }
};
