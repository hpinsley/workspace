const path = require('path');
// const HtmlWebpackPlugin = require('html-webpack-plugin');

// console.log(path.resolve(__dirname, "src"));

module.exports = {
  mode: 'development',
  stats: {
    errors: true,
    errorStack: true,
    errorDetails: true, // --display-error-details
  },
  watchOptions: {
    ignored: '**/node_modules',
    poll: 1000, // Check for changes every second
  },
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
        test: /\.scss$/,
        use: [
          // Creates `style` nodes from JS strings
          // "style-loader",
          // Translates CSS into CommonJS
          {
            loader: "css-loader",
            options: {
              modules: false,
            }
          },
          
          // Compiles Sass to CSS
          "sass-loader",
        ],
      },
      {
        test: /\.html$/i,
        loader: "html-loader",
      },
      {
        test: /\.elm$/,
          // "include" is commonly used to match the directories
        include: [
          path.resolve(__dirname, "src"),
          // path.resolve(__dirname, "test")
        ],
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          { loader: 'elm-hot-webpack-loader' },
          {
          loader: 'elm-webpack-loader',
          options: {
            // files: ['Main.elm'],
            cwd: __dirname,
            optimize: false,
            debug: true
          }
        }]
      }
    ]
  },
  devServer: {
    host: '0.0.0.0',
    hot: true,
    port: 8080,
    client: {
      logging: 'verbose',
    }
  }
};
