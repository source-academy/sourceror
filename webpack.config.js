const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "dist");

module.exports = {
  mode: "production",
  entry: {
    index: "./js/index.js"
  },
  output: {
    path: dist,
    filename: "[name].js",
    library: 'sourceror',
    libraryTarget: 'umd',
    /*library: 'sourceror',
    libraryTarget: 'umd',
    filename: 'sourceror.js',
    auxiliaryComment: 'Test Comment'*/
  },
  devServer: {
    contentBase: dist,
  },
  plugins: [
    new CopyPlugin([
      path.resolve(__dirname, "dist-static")
    ]),

    new WasmPackPlugin({
        crateDirectory: path.resolve(__dirname, "source-compiler"),
        outDir: path.resolve(__dirname, "pkg")
    }),
  ]
};
