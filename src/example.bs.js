// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var MiniTest   = require("./miniTest.bs.js");
var RereactDom = require("./rereactDom.bs.js");

function render() {
  var match = document.getElementById("container");
  if (match !== null) {
    RereactDom.render(MiniTest.createElement(/* [] */0, /* () */0), match);
    return /* () */0;
  } else {
    console.log("No dom element found :(");
    return /* () */0;
  }
}

var ParcelModule = /* module */[];

render(/* () */0);

exports.render       = render;
exports.ParcelModule = ParcelModule;
/*  Not a pure module */
