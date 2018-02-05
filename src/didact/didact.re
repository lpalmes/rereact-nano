type didactElementType =
  | Text(string)
  | Node(string);

type didactElement = {
  elementType: didactElementType,
  children: list(didactElement)
};

type didactInstance = {
  element: didactElement,
  dom: Dom.element,
  childInstances: list(didactInstance)
};

let createDomElement = (name, ~children: list(didactElement), _: unit) => {
  elementType: Node(name),
  children
};

let stringToElement = value => {elementType: Text(value), children: []};

let equalizeLists = (aList, bList) : (list(option('a)), list(option('b))) => {
  let aOptionList = List.map(a => Some(a), aList);
  let bOptionList = List.map(a => Some(a), bList);
  (aOptionList, bOptionList);
};

module DidactDom = {
  open Bs_webapi.Dom;
  let rec instantiate = (element: didactElement) : didactInstance =>
    switch element.elementType {
    | Node(name) =>
      let node = Document.createElement(name, document);
      let childInstances = List.map(instantiate, element.children);
      List.iter(
        e => {
          let _ = Element.appendChild(e.dom, node);
          ();
        },
        childInstances
      );
      {dom: node, element, childInstances};
    | Text(value) =>
      let dom = Document.createElement("span", document);
      Element.setInnerText(dom, value);
      {dom, element, childInstances: []};
    }
  and reconcile =
      (
        parentDom: Dom.element,
        instance: option(didactInstance),
        didactElement: option(didactElement)
      ) =>
    switch (instance, didactElement) {
    | (None, Some(didactElement)) =>
      let newInstance = instantiate(didactElement);
      Element.appendChild(newInstance.dom, parentDom);
      Some(newInstance);
    | (Some(instance), None) =>
      Element.removeChild(instance.dom, parentDom) |> ignore;
      None;
    | (Some({element} as instance), Some(didactElement))
        when element.elementType == didactElement.elementType =>
      Some({...instance, element: didactElement})
    | (Some(instance), Some(didactElement)) =>
      let newInstance = instantiate(didactElement);
      Element.removeChild(instance.dom, parentDom) |> ignore;
      Element.appendChild(newInstance.dom, parentDom);
      Some(newInstance);
    | (None, None) => None
    }
  and reconcileChildren =
      (instance: didactInstance, didactElement: didactElement)
      : list(didactInstance) => {
    let (a, b) =
      equalizeLists(instance.childInstances, didactElement.children);
    List.map2(reconcile(instance.dom), a, b)
    |> List.fold_left(
         (a, b) =>
           switch b {
           | Some(x) => [x, ...a]
           | None => a
           },
         []
       );
  };
};

let instance: ref(option(didactInstance)) = ref(None);

let render = (element, parentDom) => {
  let newInstance = DidactDom.reconcile(parentDom, instance^, Some(element));
  instance := newInstance;
  newInstance;
};