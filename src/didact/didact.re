type sideEffects = unit => unit;

type stateless = unit;

type actionless = unit;

module Callback = {
  type t('payload) = 'payload => unit;
  let default = _event => ();
  let chain = (handlerOne, handlerTwo, payload) => {
    handlerOne(payload);
    handlerTwo(payload);
  };
};

type props = {
  id: option(string),
  value: option(string),
  onClick: option(Dom.event => unit),
  onChange: option(Dom.event => unit)
};

let defaultProps = {id: None, value: None, onClick: None, onChange: None};

type reduce('payload, 'action) = ('payload => 'action) => Callback.t('payload);

type update('state, 'action) =
  | NoUpdate
  | Update('state)
and self('state, 'action) = {
  state: 'state,
  reduce: 'payload .reduce('payload, 'action),
  send: 'action => unit
};

type didactElementType =
  | Text(string)
  | Node(string)
  | Component(component('state, 'action)): didactElementType
and didactElement = {
  elementType: didactElementType,
  props,
  children: list(didactElement)
}
and componentSpec('state, 'initialState, 'action) = {
  debugName: string,
  render: self('state, 'action) => didactElement,
  initialState: unit => 'initialState,
  reducer: ('action, 'state) => update('state, 'action)
}
and component('state, 'action) = componentSpec('state, 'state, 'action);

let createDomElement =
    (
      name,
      ~onClick: option(Dom.event => unit)=?,
      ~children: list(didactElement),
      _: unit
    ) => {
  elementType: Node(name),
  props: {
    ...defaultProps,
    onClick
  },
  children
};

let basicComponent = debugName : componentSpec(_, _, _) => {
  debugName,
  render: _self => assert false,
  initialState: () => (),
  reducer: (_action, _state) => NoUpdate
};

let statelessComponent = debugName => {
  ...basicComponent(debugName),
  initialState: () => ()
};

let statefulComponent = debugName => basicComponent(debugName);

let reducerComponent = debugName => basicComponent(debugName);

let stringToElement = value => {
  elementType: Text(value),
  props: defaultProps,
  children: []
};

let component = component => {
  elementType: Component(component),
  props: defaultProps,
  children: []
};