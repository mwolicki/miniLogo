var path = require("path");
var webpack = require("webpack");

module.exports = {
    mode: "development",
    entry: "./tests/Tests.fsproj",
    output: {
        path: path.join(__dirname, "./tests/bin"),
        filename: "bundle.js",
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    },
    plugins: [
        new webpack.NamedModulesPlugin()
    ]
}