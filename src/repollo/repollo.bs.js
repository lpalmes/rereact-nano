// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var Block           = require("bs-platform/lib/js/block.js");
var Curry           = require("bs-platform/lib/js/curry.js");
var Fetch           = require("bs-fetch/src/Fetch.js");
var Js_dict         = require("bs-platform/lib/js/js_dict.js");
var Js_json         = require("bs-platform/lib/js/js_json.js");
var Rereact         = require("../rereact/rereact.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var RereactElements = require("../rereact/rereactElements.bs.js");

var Graphql_error = Caml_exceptions.create("Repollo.Graphql_error");

function createElement(query, render, _, _$1) {
  return Rereact.element(/* record */[
              /* debugName */"MiniTest",
              /* render */(function (param) {
                  var state = param[/* state */0];
                  if (typeof state === "number") {
                    return RereactElements.div(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* :: */[
                                Rereact.stringToElement("loading"),
                                /* [] */0
                              ], /* () */0);
                  } else if (state.tag) {
                    return RereactElements.div(/* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* :: */[
                                Rereact.stringToElement(state[0]),
                                /* [] */0
                              ], /* () */0);
                  } else {
                    return Curry._1(render, state[0]);
                  }
                }),
              /* initialState */(function () {
                  return /* Loading */0;
                }),
              /* didMount */(function (self) {
                  fetch("https://api.appetit.com.ar/graph", Fetch.RequestInit[/* make */0](/* Some */[/* Post */2], /* Some */[/* array */[/* tuple */[
                                    "content-type",
                                    "application/json"
                                  ]]], /* Some */[JSON.stringify(Js_dict.fromList(/* :: */[
                                          /* tuple */[
                                            "query",
                                            query.query
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "variables",
                                              query.variables
                                            ],
                                            /* [] */0
                                          ]
                                        ]))], /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0, /* None */0)(/* () */0)).then((function (resp) {
                          if (resp.ok) {
                            return resp.json().then((function (data) {
                                          var match = Js_json.decodeObject(data);
                                          if (match) {
                                            var data$1 = Curry._1(query.parse, match[0]["data"]);
                                            Curry._1(self[/* send */2], /* SetData */Block.__(0, [data$1]));
                                            return Promise.resolve(/* () */0);
                                          } else {
                                            Curry._1(self[/* send */2], /* SetError */Block.__(1, ["Response is not an object"]));
                                            return Promise.reject([
                                                        Graphql_error,
                                                        "Response is not an object"
                                                      ]);
                                          }
                                        }));
                          } else {
                            Curry._1(self[/* send */2], /* SetError */Block.__(1, ["Request failed"]));
                            return Promise.reject([
                                        Graphql_error,
                                        "Request failed: " + resp.statusText
                                      ]);
                          }
                        }));
                  return /* () */0;
                }),
              /* reducer */(function (action, _) {
                  if (typeof action === "number") {
                    return /* Update */[/* Loading */0];
                  } else if (action.tag) {
                    return /* Update */[/* Error */Block.__(1, [action[0]])];
                  } else {
                    return /* Update */[/* Loaded */Block.__(0, [action[0]])];
                  }
                })
            ]);
}

exports.Graphql_error = Graphql_error;
exports.createElement = createElement;
/* Js_dict Not a pure module */
