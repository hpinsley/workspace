'use strict';

// Require index.html so it gets copied to dist
// let html = require('./index.html');
import htmlString from './index.html';
console.log(htmlString);
const parser = new DOMParser();
const root = parser.parseFromString(htmlString, 'text/html');
document.documentElement.replaceWith(root.documentElement);
// document.replaceChild(root.documentElement, document.documentElement);

document.addEventListener('DOMContentLoaded', function () {
    // const root = document.createElement("div");
    // root.setAttribute("id", "main");
    
    // document.body.appendChild(root);

    var Elm = require('./Main.elm');
    var mountNode = document.getElementById('main');

    // .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
    var app = Elm.Elm.Main.init({
        node: mountNode
    });
});
