open Rereact;

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
  Nested(name, {id, value, onClick, onChange}, children);

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

let rec mapReactElement = (parentElement: Dom.element, reactElement) =>
  switch reactElement {
  | Flat(l) => IFlat(List.map(reconcile(parentElement), l))
  | Nested(name, props, elements) =>
    if (name == "List") {
      INested(name, List.map(mapReactElement(parentElement), elements))
    } else {
      let node = Document.createElement(name, document);
      Element.appendChild(node, parentElement);
      addProps(node, props);
      INested(name, List.map(mapReactElement(node), elements))
    }
  }
and reconcile = (parentElement: Dom.element, element) : instance =>
  switch element {
  | Component(component) =>
    let subElements = component.render();
    let instanceSubTree = renderReactElement(parentElement, subElements);
    {component: Some(component), element, instanceSubTree, subElements, domElement: parentElement}
  | String(value) =>
    Element.setInnerText(parentElement, value);
    {
      component: None,
      element,
      instanceSubTree: IFlat([]),
      domElement: parentElement,
      subElements: Flat([])
    }
  }
and renderReactElement = (parentElement: Dom.element, reactElement) : renderedElement =>
  mapReactElement(parentElement, reactElement);

let render = (reactElement, parentElement: Dom.element) =>
  switch (Element.lastElementChild(parentElement)) {
  | Some(childElement) =>
    let _ = Element.removeChild(childElement, parentElement);
    renderReactElement(parentElement, reactElement)
  | None =>
    Js.log("No child");
    renderReactElement(parentElement, reactElement)
  };