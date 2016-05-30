// Use PhantomJS to load the URL given as a commandline argument, and write the
// HTML to stdout. We wait for the page to finish loading before writing it out,
// which means we see the result of any on-page-load Javascript, which we
// wouldn't get with something like wget or curl.

var page   = require('webpage').create();
var system = require('system');

// Make ourselves look more like a browser than a scraper
page.settings.userAgent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/28.0.1500.71 Safari/537.36';
page.viewportSize = { width: 1024, height: 768 };

// Add an error handler, since sites may have dodgy code
// Taken from http://stackoverflow.com/a/19538646/884682
handler = function(msg, trace) {
    var msgStack = ['ERROR: ' + msg];
    if (trace && trace.length) {
        msgStack.push('TRACE:');
        trace.forEach(function(t) {
            msgStack.push(' -> ' + t.file + ': ' + t.line + (t.function ? ' (in function "' + t.function + '")' : ''));
        });
    }

    system.stderr.write(msgStack.join('\n'));
};

page.onError    = handler;
phantom.onError = handler;

page.open(system.args[1], function() {
    page.evaluate(function() {
    });
});

page.onLoadFinished = function() {
    setTimeout(function() {
        system.stdout.write(page.content);
        page.render('debug.png');
        phantom.exit(0);
    }, 2000);
};
