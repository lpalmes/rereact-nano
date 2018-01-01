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
  | Nested(option(string), props, list(reactElement))
and component = {
  debugName: string,
  render: unit => reactElement
};

let statelessComponent = (debugName) => {debugName, render: () => Flat([])};

let stringToElement = (value) => Flat([String(value)]);

let nullElement = Flat([]);

let element = (component) => Flat([Component(component)]);

let listToElement = (elements) => Nested(None, defaultProps, elements);

let arrayToElement = (elements) => Nested(None, defaultProps, Array.to_list(elements));

module ReactDom = {
  open Bs_webapi.Dom;
  let createDomElement =
      (
        name,
        ~id: option(string)=?,
        ~value: option(string)=?,
        ~onClick: option((Dom.event => unit))=?,
        ~onChange: option((Dom.event => unit))=?,
        ~children: list(reactElement),
        _: unit
      ) =>
    Nested(Some(name), {id, value, onClick, onChange}, children);
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
  let addProps = (domElement: Dom.element, props) => {
    switch props.id {
    | Some(value) => ElementRe.setAttribute("id", value, domElement)
    | None => ()
    };
    switch props.value {
    | Some(value) => ElementRe.setAttribute("value", value, domElement)
    | None => ()
    };
    switch props.onClick {
    | Some(func) => Element.addEventListener("click", func, domElement)
    | None => ()
    };
    switch props.onChange {
    | Some(func) => Element.addEventListener("change", func, domElement)
    | None => ()
    }
  };
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
    | Nested(name, props, elements) =>
      switch name {
      | Some(name) =>
        let node = Document.createElement(name, document);
        Element.appendChild(node, parentElement);
        addProps(node, props);
        List.iter((e) => render(e, node), elements)
      | None => List.iter((e) => render(e, parentElement), elements)
      }
    };
};