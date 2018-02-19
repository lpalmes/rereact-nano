// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Block      = require("bs-platform/lib/js/block.js");
var MiniTest   = require("./miniTest.bs.js");
var RereactDom = require("./rereact/rereactDom.bs.js");

var instance = [/* IFlat */Block.__(0, [/* [] */0])];

var match = document.getElementById("container");

if (match !== null) {
  instance[0] = RereactDom.render(MiniTest.createElement(/* [] */0, /* () */0), match);
} else {
  console.log("No dom element found :(");
}

console.log("Rerender");

exports.instance = instance;
/* match Not a pure module */
