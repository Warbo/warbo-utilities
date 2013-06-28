// This script will be injected into every SimpleTest result page
(function() {
    // Make sure we only run on SimpleTest pages
    if (location.href.indexOf('/simpletest/verbose/') === -1) {
        return;
    }

    var codes    = {left:  37,
                    right: 39};

    var current, prev, next;

    // To ignore paths with digits we restrict ourselves to prefixes of ".html"
    var current = parseInt(location.href
                                   .match(/(\d+).html/)[1]);
    var go      = function(n) {
        location.href = location.href
                                .replace(/\d+\.html/,
                                         n + '.html');
    };

    window.onkeyup = function(e) {
        if (e.which == codes.left &&
            e.ctrlKey &&
            current > 1) {
            go(current - 1);
        }
        if (e.which == codes.right &&
            e.ctrlKey) {
            go(current + 1);
        }
    };
}());
