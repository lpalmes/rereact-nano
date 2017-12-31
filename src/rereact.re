type element =
  | String(string)
  | Component(component)
and reactElement =
  | Flat(list(element))
  | Nested(option(string), list(reactElement))
and component = {
  debugName: string,
  render: unit => reactElement
};

let statelessComponent = (debugName) => {debugName, render: () => Flat([])};

let stringToElement = (value) => Flat([String(value)]);

let element = (component) => Flat([Component(component)]);

let listToElement = (elements) => Nested(None, elements);

module ReactDom = {
  open Bs_webapi.Dom;
  let createDomElement = (name, ~children: list(reactElement), unit: unit) =>
    Nested(Some(name), children);
  let div = createDomElement("div");
  let h1 = createDomElement("h1");
  let h2 = createDomElement("h2");
  let h3 = createDomElement("h3");
  let h4 = createDomElement("h4");
  let h5 = createDomElement("h5");
  let h6 = createDomElement("h6");
  let span = createDomElement("span");
  let ul = createDomElement("ul");
  let li = createDomElement("li");
  let img = createDomElement("img");
  let button = createDomElement("button");
  let input = createDomElement("input");
  let rec render = (element: reactElement, parentElement: Dom.element) =>
    switch element {
    | Flat(elements) =>
      List.iter(
        (elm) =>
          switch elm {
          | String(value) => Element.setInnerText(parentElement, value)
          | Component(component) => render(component.render(), parentElement)
          },
        elements
      )
    | Nested(name, elements) =>
      switch name {
      | Some(name) =>
        let node = Document.createElement(name, document);
        Element.appendChild(node, parentElement);
        List.iter((e) => render(e, node), elements)
      | None => List.iter((e) => render(e, parentElement), elements)
      }
    };
};