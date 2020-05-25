const common = require('./webpack.common.js');

let appconfig = common[0];
let workerconfig = common[1];

appconfig.mode = "development"
workerconfig.mode = "development"

module.exports = [appconfig,workerconfig]
