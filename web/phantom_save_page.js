// Use PhantomJS to load the URL given as a commandline argument, and write the
// HTML to stdout. We wait for the page to finish loading before writing it out,
// which means we see the result of any on-page-load Javascript, which we
// wouldn't get with something like wget or curl.

var page   = require('webpage').create();
var system = require('system');

page.open(system.args[1], function() {
    page.evaluate(function() {
    });
});

page.onLoadFinished = function() {
    system.stdout.write(page.content);
    phantom.exit(0);
};
