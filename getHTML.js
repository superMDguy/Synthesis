var page = require('webpage').create();
var system = require('system');

page.open(system.args[1], function(status) {
  var content = page.evaluate(function() {
    return document.documentElement.innerHTML;
  });
  console.log(content)
  phantom.exit();
});