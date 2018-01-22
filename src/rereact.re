type props = {
  id: option(string),
  value: option(string),
  onClick: option((Dom.event => unit)),
  onChange: option((Dom.event => unit))
};

let defaultProps = {id: None, value: None, onClick: None, onChange: None};

type element =
  | String(string)
  | Component(component)
and reactElement =
  | Flat(list(element))
  | Nested(string, props, list(reactElement))
and component = {
  debugName: string,
  render: unit => reactElement
};

let statelessComponent = (debugName) => {debugName, render: () => Flat([])};

let stringToElement = (value) => Flat([String(value)]);

let nullElement = Flat([]);

let element = (component) => Flat([Component(component)]);

let listToElement = (elements) => Nested("List", defaultProps, elements);

let arrayToElement = (elements) => Nested("List", defaultProps, Array.to_list(elements));