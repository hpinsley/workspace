'use strict';

// Require index.html so it gets copied to dist
// let html = require('./index.html');
import htmlString from './index.html';
import styles from './index.scss';

console.log('styles:', styles);

const parser = new DOMParser();
const root = parser.parseFromString(htmlString, 'text/html');
document.documentElement.replaceWith(root.documentElement);

function addInlineCSS(cssContent) {
    const style = document.createElement('style');
    style.textContent = cssContent;

    // Append the style element to the <head> or <documentElement>
    document.documentElement.appendChild(style);
}

const cssContent = styles[0][1];
console.log(cssContent);
addInlineCSS(cssContent)

document.addEventListener('DOMContentLoaded', function () {
    var Elm = require('./Main.elm');
    var mountNode = document.getElementById('main');

    // .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
    var app = Elm.Elm.Main.init({
        node: mountNode
    });
});
