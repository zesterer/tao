const common = require('./webpack.common.js');

let appconfig = common[0];
let workerconfig = common[1];

appconfig.mode = "production"
workerconfig.mode = "production"

module.exports = [appconfig,workerconfig]