'use strict';

require('./index.html');
var Elm = require('./Main');

var myapp = Elm.Picross.embed(document.getElementById('main'));

//var myapp = Elm.Picross.fullscreen();
myapp.ports.requestBoardMousePos.subscribe(requestBoardMousePos);

function requestBoardMousePos(pos) {
    var board = document.getElementById('board');
    if (!board) {
        return
    }
    var rect = board.getBoundingClientRect();
    // back to Elm
    myapp.ports.boardMousePosResult.send([ pos[0] - rect.left, pos[1] - rect.top ]);
}
