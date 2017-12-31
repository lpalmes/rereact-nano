const path = require('path')

module.exports = {
  entry: './src/example.bs.js',
  output: {
    path: path.join(__dirname, 'src/build'),
    filename: '[name].js'
  }
}
