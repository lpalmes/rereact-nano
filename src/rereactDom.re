open Rereact;

open Bs_webapi.Dom;

type renderedElement =
  | IFlat(list(opaqueInstance))
  | INested(string, list(renderedElement), Dom.element)
and instance('state, 'action) = {
  component: option(component('state, 'action)),
  element,
  iState: 'state,
  instanceSubTree: renderedElement,
  domElement: Dom.element,
  subElements: reactElement,
  pendingStateUpdates: ref(list(('state => update('state, 'action))))
}
and opaqueInstance =
  | Instance(instance('state, 'action)): opaqueInstance;

let globalInstance = ref(IFlat([]));

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

let createSelf = (~instance) : self(_) => {
  state: instance.iState,
  reduce: (payloadToAction, payload) =>
    switch instance.component {
    | Some(component) =>
      let action = payloadToAction(payload);
      let stateUpdate = component.reducer(action);
      instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^]
    | _ => ()
    },
  send: (action) =>
    switch instance.component {
    | Some(component) =>
      let stateUpdate = component.reducer(action);
      instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^];
      Js.log(Array.of_list(instance.pendingStateUpdates^))
    | _ => ()
    }
};

let createInstance = (~component, ~element, ~instanceSubTree, ~subElements) => {
  let iState = component.initialState();
  {
    component: Some(component),
    element,
    domElement: Document.createElement("span", document),
    iState,
    instanceSubTree,
    subElements,
    pendingStateUpdates: ref([])
  }
};

let rec mapRenderedElement = (f, renderedElement) =>
  switch renderedElement {
  | IFlat(l) =>
    let nextL = List.map(f, l);
    let unchanged = List.for_all2((===), l, nextL);
    unchanged ? renderedElement : IFlat(nextL)
  | INested(s, l, d) =>
    let nextL = List.map(mapRenderedElement(f), l);
    let unchanged = List.for_all2((===), l, nextL);
    unchanged ? renderedElement : INested(s, nextL, d)
  };

let rec renderReactElement = (parentElement: Dom.element, reactElement) : renderedElement =>
  switch reactElement {
  | Flat(l) => IFlat(List.map(reconcile(parentElement), l))
  | Nested(name, props, elements) =>
    if (name == "List") {
      INested(
        name,
        List.map(renderReactElement(parentElement), elements),
        Document.createElement("span", document)
      )
    } else {
      let node = Document.createElement(name, document);
      Element.appendChild(node, parentElement);
      addProps(node, props);
      INested(name, List.map(renderReactElement(node), elements), node)
    }
  }
and reconcile = (parentElement: Dom.element, element) : opaqueInstance =>
  switch element {
  | Component(component) =>
    let instance =
      createInstance(~component, ~element, ~instanceSubTree=IFlat([]), ~subElements=Flat([]));
    let self = createSelf(~instance);
    let subElements = component.render(self);
    let instanceSubTree = renderReactElement(parentElement, subElements);
    Instance({...instance, instanceSubTree, subElements, domElement: parentElement})
  | String(value) =>
    Element.setInnerText(parentElement, value);
    Instance({
      component: None,
      element,
      iState: (),
      instanceSubTree: IFlat([]),
      domElement: parentElement,
      subElements: Flat([]),
      pendingStateUpdates: ref([])
    })
  };

let executePendingStateUpdates = (opaqueInstance) => {
  let Instance(instance) = opaqueInstance;
  let executeUpdate = (~state, stateUpdate) =>
    switch (stateUpdate(state)) {
    | NoUpdate => state
    | Update(newState) => newState
    };
  let rec executeUpdates = (~state, stateUpdates) =>
    switch stateUpdates {
    | [] => state
    | [stateUpdate, ...otherStateUpdates] =>
      let nextState = executeUpdate(~state, stateUpdate);
      executeUpdates(~state=nextState, otherStateUpdates)
    };
  let pendingUpdates = List.rev(instance.pendingStateUpdates^);
  instance.pendingStateUpdates := [];
  let nextState = executeUpdates(~state=instance.iState, pendingUpdates);
  instance.iState === nextState ? opaqueInstance : Instance({...instance, iState: nextState})
};

let flushPendingUpdatesFromInstance = (instance) => executePendingStateUpdates(instance);

let flushPendingUpdates = (renderedElement) =>
  mapRenderedElement(flushPendingUpdatesFromInstance, renderedElement);

let rec renderRenderedElement = (renderedElement) =>
  switch renderedElement {
  | IFlat(l) => IFlat(l)
  | INested(name, elements, d) =>
    if (name == "List") {
      INested(name, List.map(renderRenderedElement, elements), d)
    } else {
      let node = Document.createElement(name, document);
      Element.appendChild(node, d);
      INested(name, List.map(renderRenderedElement, elements), node)
    }
  }
and reconcileRenderedElement = (Instance(instance)) : opaqueInstance => {
  let {domElement, element} = instance;
  switch element {
  | Component(component) =>
    let self = createSelf(~instance);
    let subElements = component.render(self);
    let instanceSubTree = renderReactElement(domElement, subElements);
    Instance({...instance, instanceSubTree, subElements, domElement})
  | String(value) =>
    Element.setInnerText(domElement, value);
    Instance({
      component: None,
      element,
      iState: (),
      instanceSubTree: IFlat([]),
      domElement,
      subElements: Flat([]),
      pendingStateUpdates: ref([])
    })
  }
};

let rerender = (renderedElement) => {
  let rendered = flushPendingUpdates(renderedElement);
  Js.log(rendered)
};

let render = (reactElement, parentElement: Dom.element) => {
  let instance =
    switch (Element.lastElementChild(parentElement)) {
    | Some(childElement) =>
      let _ = Element.removeChild(childElement, parentElement);
      renderReactElement(parentElement, reactElement)
    | None => renderReactElement(parentElement, reactElement)
    };
  globalInstance := instance;
  instance
};