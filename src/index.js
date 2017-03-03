'use strict';

require('./index.html');

var Elm = require('./Main');

var localStorageKey = 'picross-elm';

function init() {
    var ret = {
        doneLevels: [],
        foo: 2
    };
    if (window.localStorage) {
        try {
            var obj = JSON.parse(localStorage.getItem(localStorageKey));
            ret.doneLevels = obj[0] || [];
            ret.foo = 4;
        } catch (e) {}
    }

    return ret;
}

var myapp = Elm.Picross.embed(document.getElementById('main'), init());

myapp.ports.computeBoardSize.subscribe(computeBoardSize);
myapp.ports.requestSvgMousePos.subscribe(requestSvgMousePos);
myapp.ports.save.subscribe(save);

function computeBoardSize() {
    var board = document.getElementById('board');
    if (!board) {
        return;
    }
    window.requestAnimationFrame(function() {
        var bbox = board.getBBox();
        var rect = board.getBoundingClientRect();
        myapp.ports.computeBoardSizeResult.send([bbox.x, bbox.y, Math.max(bbox.width, rect.width), Math.max(bbox.height, rect.height)]);
    }, 10);
}

function requestSvgMousePos(pos) {
    var svg = document.getElementById('board');
    var rect = svg.getBoundingClientRect();
    var pt = svg.createSVGPoint();
    pt.x = pos[0];
    pt.y = pos[1];
    var p = pt.matrixTransform(svg.getScreenCTM().inverse());
    myapp.ports.svgMousePosResult.send([p.x, p.y]);
}

function save(data) {
    if (window.localStorage) {
        localStorage.setItem(localStorageKey, JSON.stringify(data));
    }
}
