'use strict';

require('./index.html');

var Elm = require('./Main');

var myapp = Elm.Picross.embed(document.getElementById('main'));

myapp.ports.computeBoardSize.subscribe(computeBoardSize);
myapp.ports.requestTransMousePos.subscribe(requestTransMousePos);

var installed;
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
    installRightClickHandler(svg);
    pt = svg.createSVGPoint();
    pt.x = pos[0];
    pt.y = pos[1];
    var p = pt.matrixTransform(svg.getScreenCTM().inverse());
    myapp.ports.transMousePosResult.send([p.x, p.y]);
}

function installRightClickHandler(board) {
    if (installed) {
        return;
    }
    board.addEventListener('mousedown', function (ev) {
        ev.preventDefault();
        if (ev.which === 1) {
            myapp.ports.boardMouseDown.send(1);
        } else if (ev.which === 3) {
            myapp.ports.boardMouseDown.send(3);
        }
        return false;
    }, false);
    board.addEventListener('mouseup', function (ev) {
        ev.preventDefault();
        if (ev.which === 1) {
            myapp.ports.boardMouseUp.send(1);
        } else if (ev.which === 3) {
            myapp.ports.boardMouseUp.send(3);
        }
        return false;
    }, false);
    board.addEventListener('contextmenu', function (ev) {
        ev.preventDefault();
        return false;
    }, false);
    installed = true;
}
