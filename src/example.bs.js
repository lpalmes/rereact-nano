// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List       = require("bs-platform/lib/js/list.js");
var Rereact    = require("./rereact.bs.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var RereactDom = require("./rereactDom.bs.js");

var component = Rereact.statelessComponent("Test");

function createElement($staropt$star, children, _) {
  var name = $staropt$star ? $staropt$star[0] : "Reason";
  return Rereact.element(/* record */[
              /* debugName */component[/* debugName */0],
              /* render */(function () {
                  return RereactDom.div(/* None */0, /* None */0, /* None */0, /* None */0, /* :: */[
                              RereactDom.h4(/* None */0, /* None */0, /* None */0, /* None */0, /* :: */[
                                    Rereact.stringToElement(name),
                                    /* [] */0
                                  ], /* () */0),
                              /* :: */[
                                Rereact.listToElement(children),
                                /* [] */0
                              ]
                            ], /* () */0);
                })
            ]);
}

var Test = /* module */[
  /* component */component,
  /* createElement */createElement
];

var component$1 = Rereact.statelessComponent("Sample");

function createElement$1(_, _$1, _$2) {
  return Rereact.element(/* record */[
              /* debugName */component$1[/* debugName */0],
              /* render */(function () {
                  return RereactDom.div(/* None */0, /* None */0, /* None */0, /* None */0, /* :: */[
                              Rereact.listToElement(List.map((function (e) {
                                          return RereactDom.div(/* None */0, /* None */0, /* None */0, /* None */0, /* :: */[
                                                      Rereact.stringToElement(Pervasives.string_of_int(e)),
                                                      /* [] */0
                                                    ], /* () */0);
                                        }), /* :: */[
                                        1,
                                        /* :: */[
                                          2,
                                          /* :: */[
                                            3,
                                            /* :: */[
                                              4,
                                              /* :: */[
                                                5,
                                                /* [] */0
                                              ]
                                            ]
                                          ]
                                        ]
                                      ])),
                              /* :: */[
                                createElement(/* None */0, /* :: */[
                                      RereactDom.button(/* None */0, /* None */0, /* Some */[(function () {
                                                console.log("hello");
                                                return /* () */0;
                                              })], /* None */0, /* :: */[
                                            Rereact.stringToElement("click me"),
                                            /* [] */0
                                          ], /* () */0),
                                      /* [] */0
                                    ], /* () */0),
                                /* :: */[
                                  RereactDom.input(/* None */0, /* Some */["Hello my friend"], /* None */0, /* Some */[(function (e) {
                                            console.log(e);
                                            return /* () */0;
                                          })], /* [] */0, /* () */0),
                                  /* [] */0
                                ]
                              ]
                            ], /* () */0);
                })
            ]);
}

var Sample = /* module */[
  /* component */component$1,
  /* createElement */createElement$1
];

var numberOfRenders = [0];

function render() {
  var match = document.getElementById("container");
  if (match !== null) {
    var instance = RereactDom.render(createElement$1("Test", /* [] */0, /* () */0), match);
    console.log(instance);
    return /* () */0;
  } else {
    console.log("No dom element found :(");
    return /* () */0;
  }
}

var ParcelModule = /* module */[];

render(/* () */0);

module.hot.dispose((function () {
        var newValue = numberOfRenders[0] + 1 | 0;
        numberOfRenders[0] = newValue;
        return /* () */0;
      }));

module.hot.accept((function () {
        render(/* () */0);
        numberOfRenders[0] = numberOfRenders[0] + 1 | 0;
        console.log(numberOfRenders);
        return /* () */0;
      }));

exports.Test            = Test;
exports.Sample          = Sample;
exports.numberOfRenders = numberOfRenders;
exports.render          = render;
exports.ParcelModule    = ParcelModule;
/* component Not a pure module */
