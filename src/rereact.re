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
  type renderedElement =
    | IFlat(list(instance))
    | INested(string, list(renderedElement))
  and instance = {
    component: option(component),
    element,
    instanceSubTree: renderedElement,
    domElement: Dom.element,
    subElements: reactElement
  };
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
  let rec reconcile =
          (element: reactElement, parentElement: Dom.element, instance: option(renderedElement))
          : renderedElement =>
    switch element {
    | Flat(elements) =>
      List.map(
        (elm) =>
          switch elm {
          | String(value) =>
            Element.setInnerText(parentElement, value);
            IFlat([
              {
                component: None,
                element: elm,
                instanceSubTree: IFlat([]),
                domElement: parentElement,
                subElements: Flat([])
              }
            ])
          | Component(component) => reconcile(component.render(), parentElement, None)
          },
        elements
      )
    | Nested(name, props, elements) =>
      switch name {
      | Some(name) =>
        let node = Document.createElement(name, document);
        Element.appendChild(node, parentElement);
        addProps(node, props);
        List.iter((e) => reconcile(e, node), elements)
      | None => List.iter((e) => reconcile(e, parentElement), elements)
      }
    };
  let render = (element: reactElement, parentElement: Dom.element) =>
    switch (Element.lastElementChild(parentElement)) {
    | Some(childElement) =>
      Js.log(childElement);
      let _ = Element.removeChild(childElement, parentElement);
      reconcile(element, parentElement)
    | None =>
      Js.log("No child");
      reconcile(element, parentElement)
    };
};