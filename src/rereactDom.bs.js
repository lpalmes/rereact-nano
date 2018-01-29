// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List    = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block   = require("bs-platform/lib/js/block.js");
var Curry   = require("bs-platform/lib/js/curry.js");

function createDomElement(name, id, value, onClick, onChange, children, _) {
  return /* Nested */Block.__(1, [
            name,
            /* record */[
              /* id */id,
              /* value */value,
              /* onClick */onClick,
              /* onChange */onChange
            ],
            children
          ]);
}

function div(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("div", param, param$1, param$2, param$3, param$4, param$5);
}

function h1(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("h1", param, param$1, param$2, param$3, param$4, param$5);
}

function h2(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("h2", param, param$1, param$2, param$3, param$4, param$5);
}

function h3(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("h3", param, param$1, param$2, param$3, param$4, param$5);
}

function h4(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("h4", param, param$1, param$2, param$3, param$4, param$5);
}

function h5(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("h5", param, param$1, param$2, param$3, param$4, param$5);
}

function h6(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("h6", param, param$1, param$2, param$3, param$4, param$5);
}

function span(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("span", param, param$1, param$2, param$3, param$4, param$5);
}

function ul(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("ul", param, param$1, param$2, param$3, param$4, param$5);
}

function li(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("li", param, param$1, param$2, param$3, param$4, param$5);
}

function img(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("img", param, param$1, param$2, param$3, param$4, param$5);
}

function button(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("button", param, param$1, param$2, param$3, param$4, param$5);
}

function input(param, param$1, param$2, param$3, param$4, param$5) {
  return createDomElement("input", param, param$1, param$2, param$3, param$4, param$5);
}

function addProps(domElement, props) {
  var match = props[/* id */0];
  if (match) {
    domElement.setAttribute("id", match[0]);
  }
  var match$1 = props[/* value */1];
  if (match$1) {
    domElement.setAttribute("value", match$1[0]);
  }
  var match$2 = props[/* onClick */2];
  if (match$2) {
    domElement.addEventListener("click", match$2[0]);
  }
  var match$3 = props[/* onChange */3];
  if (match$3) {
    domElement.addEventListener("change", match$3[0]);
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function createSelf(instance) {
  return /* record */[
          /* state */instance[/* iState */2],
          /* reduce */(function (payloadToAction, payload) {
              var match = instance[/* component */0];
              if (match) {
                var action = Curry._1(payloadToAction, payload);
                var stateUpdate = Curry._1(match[0][/* reducer */3], action);
                instance[/* pendingStateUpdates */6][0] = /* :: */[
                  stateUpdate,
                  instance[/* pendingStateUpdates */6][0]
                ];
                return /* () */0;
              } else {
                return /* () */0;
              }
            }),
          /* send */(function (action) {
              var match = instance[/* component */0];
              if (match) {
                var stateUpdate = Curry._1(match[0][/* reducer */3], action);
                instance[/* pendingStateUpdates */6][0] = /* :: */[
                  stateUpdate,
                  instance[/* pendingStateUpdates */6][0]
                ];
                console.log($$Array.of_list(instance[/* pendingStateUpdates */6][0]));
                return /* () */0;
              } else {
                return /* () */0;
              }
            })
        ];
}

function createInstance(component, element, instanceSubTree, subElements) {
  var iState = Curry._1(component[/* initialState */2], /* () */0);
  return /* record */[
          /* component : Some */[component],
          /* element */element,
          /* iState */iState,
          /* instanceSubTree */instanceSubTree,
          /* domElement */document.createElement("span"),
          /* subElements */subElements,
          /* pendingStateUpdates */[/* [] */0]
        ];
}

function mapReactElement(f, reactElement) {
  if (reactElement.tag) {
    return /* INested */Block.__(1, [
              reactElement[0],
              List.map((function (param) {
                      return mapReactElement(f, param);
                    }), reactElement[2])
            ]);
  } else {
    return /* IFlat */Block.__(0, [List.map(f, reactElement[0])]);
  }
}

function mapRenderedElement(f, renderedElement) {
  if (renderedElement.tag) {
    var l = renderedElement[1];
    var nextL = List.map((function (param) {
            return mapRenderedElement(f, param);
          }), l);
    var unchanged = List.for_all2((function (prim, prim$1) {
            return +(prim === prim$1);
          }), l, nextL);
    if (unchanged !== 0) {
      return renderedElement;
    } else {
      return /* INested */Block.__(1, [
                renderedElement[0],
                nextL
              ]);
    }
  } else {
    var l$1 = renderedElement[0];
    var nextL$1 = List.map(f, l$1);
    var unchanged$1 = List.for_all2((function (prim, prim$1) {
            return +(prim === prim$1);
          }), l$1, nextL$1);
    if (unchanged$1 !== 0) {
      return renderedElement;
    } else {
      return /* IFlat */Block.__(0, [nextL$1]);
    }
  }
}

function renderReactElement(parentElement, reactElement) {
  if (reactElement.tag) {
    var elements = reactElement[2];
    var name = reactElement[0];
    if (name === "List") {
      return /* INested */Block.__(1, [
                name,
                List.map((function (param) {
                        return renderReactElement(parentElement, param);
                      }), elements)
              ]);
    } else {
      var node = document.createElement(name);
      parentElement.appendChild(node);
      addProps(node, reactElement[1]);
      return /* INested */Block.__(1, [
                name,
                List.map((function (param) {
                        return renderReactElement(node, param);
                      }), elements)
              ]);
    }
  } else {
    return /* IFlat */Block.__(0, [List.map((function (param) {
                      return reconcile(parentElement, param);
                    }), reactElement[0])]);
  }
}

function reconcile(parentElement, element) {
  if (element.tag) {
    var component = element[0];
    var instance = createInstance(component, element, /* IFlat */Block.__(0, [/* [] */0]), /* Flat */Block.__(0, [/* [] */0]));
    var self = createSelf(instance);
    var subElements = Curry._1(component[/* render */1], self);
    var instanceSubTree = renderReactElement(parentElement, subElements);
    return /* Instance */[/* record */[
              /* component */instance[/* component */0],
              /* element */instance[/* element */1],
              /* iState */instance[/* iState */2],
              /* instanceSubTree */instanceSubTree,
              /* domElement */parentElement,
              /* subElements */subElements,
              /* pendingStateUpdates */instance[/* pendingStateUpdates */6]
            ]];
  } else {
    parentElement.innerText = element[0];
    return /* Instance */[/* record */[
              /* component : None */0,
              /* element */element,
              /* iState : () */0,
              /* instanceSubTree : IFlat */Block.__(0, [/* [] */0]),
              /* domElement */parentElement,
              /* subElements : Flat */Block.__(0, [/* [] */0]),
              /* pendingStateUpdates */[/* [] */0]
            ]];
  }
}

var globalInstance = [/* IFlat */Block.__(0, [/* [] */0])];

function executePendingStateUpdates(opaqueInstance) {
  var instance = opaqueInstance[0];
  var executeUpdate = function (state, stateUpdate) {
    var match = Curry._1(stateUpdate, state);
    if (match) {
      return match[0];
    } else {
      return state;
    }
  };
  var executeUpdates = function (_state, _stateUpdates) {
    while(true) {
      var stateUpdates = _stateUpdates;
      var state = _state;
      if (stateUpdates) {
        var nextState = executeUpdate(state, stateUpdates[0]);
        _stateUpdates = stateUpdates[1];
        _state = nextState;
        continue ;
        
      } else {
        return state;
      }
    };
  };
  var pendingUpdates = List.rev(instance[/* pendingStateUpdates */6][0]);
  instance[/* pendingStateUpdates */6][0] = /* [] */0;
  var nextState = executeUpdates(instance[/* iState */2], pendingUpdates);
  var match = +(instance[/* iState */2] === nextState);
  if (match !== 0) {
    return opaqueInstance;
  } else {
    var newrecord = instance.slice();
    return /* Instance */[(newrecord[/* iState */2] = nextState, newrecord)];
  }
}

var flushPendingUpdatesFromInstance = executePendingStateUpdates;

function flushPendingUpdates(renderedElement) {
  return mapRenderedElement(flushPendingUpdatesFromInstance, renderedElement);
}

function rerender(renderedElement) {
  if (renderedElement.tag) {
    return List.iter(rerender, renderedElement[1]);
  } else {
    return List.iter((function () {
                  return /* () */0;
                }), renderedElement[0]);
  }
}

function render(reactElement, parentElement) {
  var match = parentElement.lastElementChild;
  var instance = match !== null ? (parentElement.removeChild(match), renderReactElement(parentElement, reactElement)) : renderReactElement(parentElement, reactElement);
  globalInstance[0] = instance;
  return instance;
}

exports.createDomElement                = createDomElement;
exports.div                             = div;
exports.h1                              = h1;
exports.h2                              = h2;
exports.h3                              = h3;
exports.h4                              = h4;
exports.h5                              = h5;
exports.h6                              = h6;
exports.span                            = span;
exports.ul                              = ul;
exports.li                              = li;
exports.img                             = img;
exports.button                          = button;
exports.input                           = input;
exports.addProps                        = addProps;
exports.createSelf                      = createSelf;
exports.createInstance                  = createInstance;
exports.mapReactElement                 = mapReactElement;
exports.mapRenderedElement              = mapRenderedElement;
exports.renderReactElement              = renderReactElement;
exports.reconcile                       = reconcile;
exports.globalInstance                  = globalInstance;
exports.executePendingStateUpdates      = executePendingStateUpdates;
exports.flushPendingUpdatesFromInstance = flushPendingUpdatesFromInstance;
exports.flushPendingUpdates             = flushPendingUpdates;
exports.rerender                        = rerender;
exports.render                          = render;
/* No side effect */
