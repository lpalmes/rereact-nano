// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List  = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function statelessComponent(debugName) {
  return /* record */[
          /* debugName */debugName,
          /* render */(function () {
              return /* Flat */Block.__(0, [/* [] */0]);
            })
        ];
}

function stringToElement(value) {
  return /* Flat */Block.__(0, [/* :: */[
              /* String */Block.__(0, [value]),
              /* [] */0
            ]]);
}

function element(component) {
  return /* Flat */Block.__(0, [/* :: */[
              /* Component */Block.__(1, [component]),
              /* [] */0
            ]]);
}

function listToElement(elements) {
  return /* Nested */Block.__(1, [
            /* None */0,
            elements
          ]);
}

function createDomElement(name, children, _) {
  return /* Nested */Block.__(1, [
            /* Some */[name],
            children
          ]);
}

function div(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["div"],
            param
          ]);
}

function h1(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["h1"],
            param
          ]);
}

function h2(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["h2"],
            param
          ]);
}

function h3(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["h3"],
            param
          ]);
}

function h4(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["h4"],
            param
          ]);
}

function h5(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["h5"],
            param
          ]);
}

function h6(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["h6"],
            param
          ]);
}

function span(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["span"],
            param
          ]);
}

function ul(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["ul"],
            param
          ]);
}

function li(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["li"],
            param
          ]);
}

function img(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["img"],
            param
          ]);
}

function button(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["button"],
            param
          ]);
}

function input(param, _) {
  return /* Nested */Block.__(1, [
            /* Some */["input"],
            param
          ]);
}

function render(element, parentElement) {
  if (element.tag) {
    var elements = element[1];
    var name = element[0];
    if (name) {
      var node = document.createElement(name[0]);
      parentElement.appendChild(node);
      return List.iter((function (e) {
                    return render(e, node);
                  }), elements);
    } else {
      return List.iter((function (e) {
                    return render(e, parentElement);
                  }), elements);
    }
  } else {
    return List.iter((function (elm) {
                  if (elm.tag) {
                    return render(Curry._1(elm[0][/* render */1], /* () */0), parentElement);
                  } else {
                    parentElement.innerText = elm[0];
                    return /* () */0;
                  }
                }), element[0]);
  }
}

var ReactDom = /* module */[
  /* createDomElement */createDomElement,
  /* div */div,
  /* h1 */h1,
  /* h2 */h2,
  /* h3 */h3,
  /* h4 */h4,
  /* h5 */h5,
  /* h6 */h6,
  /* span */span,
  /* ul */ul,
  /* li */li,
  /* img */img,
  /* button */button,
  /* input */input,
  /* render */render
];

exports.statelessComponent = statelessComponent;
exports.stringToElement    = stringToElement;
exports.element            = element;
exports.listToElement      = listToElement;
exports.ReactDom           = ReactDom;
/* No side effect */
