// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var $$Array                 = require("bs-platform/lib/js/array.js");
var Block                   = require("bs-platform/lib/js/block.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var defaultProps = /* record */[
  /* id : None */0,
  /* value : None */0,
  /* onClick : None */0,
  /* onChange : None */0
];

function $$default() {
  return /* () */0;
}

function chain(handlerOne, handlerTwo, payload) {
  Curry._1(handlerOne, payload);
  return Curry._1(handlerTwo, payload);
}

var Callback = /* module */[
  /* default */$$default,
  /* chain */chain
];

function basicComponent(debugName) {
  return /* record */[
          /* debugName */debugName,
          /* render */(function () {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    [
                      "rereact.re",
                      52,
                      21
                    ]
                  ];
            }),
          /* initialState */(function () {
              return /* () */0;
            }),
          /* reducer */(function (_, _$1) {
              return /* NoUpdate */0;
            })
        ];
}

function statelessComponent(debugName) {
  var init = basicComponent(debugName);
  return /* record */[
          /* debugName */init[/* debugName */0],
          /* render */init[/* render */1],
          /* initialState */(function () {
              return /* () */0;
            }),
          /* reducer */init[/* reducer */3]
        ];
}

var statefulComponent = basicComponent;

var reducerComponent = basicComponent;

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
            "List",
            defaultProps,
            elements
          ]);
}

function arrayToElement(elements) {
  return /* Nested */Block.__(1, [
            "List",
            defaultProps,
            $$Array.to_list(elements)
          ]);
}

var nullElement = /* Flat */Block.__(0, [/* [] */0]);

exports.defaultProps       = defaultProps;
exports.Callback           = Callback;
exports.basicComponent     = basicComponent;
exports.statelessComponent = statelessComponent;
exports.statefulComponent  = statefulComponent;
exports.reducerComponent   = reducerComponent;
exports.stringToElement    = stringToElement;
exports.nullElement        = nullElement;
exports.element            = element;
exports.listToElement      = listToElement;
exports.arrayToElement     = arrayToElement;
/* No side effect */
