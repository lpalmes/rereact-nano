type props = {
  id: option(string),
  value: option(string),
  onClick: option((Dom.event => unit)),
  onChange: option((Dom.event => unit))
};

let defaultProps = {id: None, value: None, onClick: None, onChange: None};

type sideEffects = unit => unit;

type stateless = unit;

type actionless = unit;

module Callback = {
  type t('payload) = 'payload => unit;
  let default = (_event) => ();
  let chain = (handlerOne, handlerTwo, payload) => {
    handlerOne(payload);
    handlerTwo(payload)
  };
};

type reduce('payload, 'action) = ('payload => 'action) => Callback.t('payload);

type update('state, 'action) =
  | NoUpdate
  | Update('state)
and self('state, 'action) = {
  state: 'state,
  reduce: 'payload .reduce('payload, 'action),
  send: 'action => unit
};

type element =
  | String(string)
  | Component(component('state, 'action)): element
and reactElement =
  | Flat(list(element))
  | Nested(string, props, list(reactElement))
and componentSpec('state, 'initialState, 'action) = {
  debugName: string,
  render: self('state, 'action) => reactElement,
  initialState: unit => 'initialState,
  reducer: ('action, 'state) => update('state, 'action)
}
and component('state, 'action) = componentSpec('state, 'state, 'action);

let basicComponent = (debugName) : componentSpec(_, _, _) => {
  debugName,
  render: (_self) => assert false,
  initialState: () => (),
  reducer: (_action, _state) => NoUpdate
};

let statelessComponent = (debugName) => {...basicComponent(debugName), initialState: () => ()};

let statefulComponent = (debugName) => basicComponent(debugName);

let reducerComponent = (debugName) => basicComponent(debugName);

let stringToElement = (value) => Flat([String(value)]);

let nullElement = Flat([]);

let element = (component) => Flat([Component(component)]);

let listToElement = (elements) => Nested("List", defaultProps, elements);

let arrayToElement = (elements) => Nested("List", defaultProps, Array.to_list(elements));