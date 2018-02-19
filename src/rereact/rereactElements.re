open Rereact;

let createDomElement =
    (
      name,
      ~id: option(string)=?,
      ~value: option(string)=?,
      ~onClick: option(unit => unit)=?,
      ~onChange: option(unit => unit)=?,
      ~className: option(string)=?,
      ~children: list(reactElement),
      _: unit
    ) =>
  Nested(name, {id, value, onClick, className, onChange}, children);

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