'use strict';

require('./index.html');

var Elm = require('./Main');

var myapp = Elm.Picross.embed(document.getElementById('main'));

myapp.ports.computeBoardSize.subscribe(computeBoardSize);
myapp.ports.requestTransMousePos.subscribe(requestTransMousePos);
myapp.ports.requestTransMousePos2.subscribe(requestTransMousePos2);

var pt;

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

function requestTransMousePos(pos) {
    var svg = document.getElementById('board');
    pt = svg.createSVGPoint();
    pt.x = pos[0];
    pt.y = pos[1];
    // console.log('requestTransMousePos', pos[0], pos[1]);
    var p = pt.matrixTransform(svg.getScreenCTM().inverse());
    myapp.ports.transMousePosResult.send([p.x, p.y]);
}

function requestTransMousePos2(pos) {
    var svg = document.getElementById('board');
    var rect = svg.getBoundingClientRect();
    pt = svg.createSVGPoint();
    pt.x = pos[0]; //- rect.left;
    pt.y = pos[1]; // - rect.top;
    // console.log('requestTransMousePos2', pos[0] - rect.left, pos[1] - rect.top);
    var p = pt.matrixTransform(svg.getScreenCTM().inverse());
    myapp.ports.transMousePosResult2.send([p.x, p.y]);
}
