'use strict';

require('./index.html');
var Elm = require('./Main');

var myapp = Elm.Picross.embed(document.getElementById('main'));

//var myapp = Elm.Picross.fullscreen();
myapp.ports.requestBoardMousePos.subscribe(requestBoardMousePos);

var installed;

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

function requestBoardMousePos(pos) {
    var board = document.getElementById('board');
    if (!board) {
        return;
    }
    installRightClickHandler(board);
    var rect = board.getBoundingClientRect();
    // back to Elm
    myapp.ports.boardMousePosResult.send([ pos[0] - rect.left, pos[1] - rect.top ]);
}
