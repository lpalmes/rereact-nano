open Rereact;

open Bs_webapi.Dom;

type renderedElement =
  | IFlat(list(opaqueInstance))
  | INested(string, list(renderedElement), option(Dom.element))
and instance('state, 'action) = {
  component: option(component('state, 'action)),
  element,
  iState: 'state,
  instanceSubTree: renderedElement,
  domElement: option(Dom.element),
  subElements: reactElement,
  pendingStateUpdates: ref(list('state => update('state, 'action)))
}
and opaqueInstance =
  | Instance(instance('state, 'action)): opaqueInstance;

let globalInstance = ref(IFlat([]));

let createDomElement =
    (
      name,
      ~id: option(string)=?,
      ~value: option(string)=?,
      ~onClick: option(Dom.event => unit)=?,
      ~onChange: option(Dom.event => unit)=?,
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
  };
};

let createSelf = (~instance) : self(_) => {
  state: instance.iState,
  reduce: (payloadToAction, payload) =>
    switch instance.component {
    | Some(component) =>
      let action = payloadToAction(payload);
      let stateUpdate = component.reducer(action);
      instance.pendingStateUpdates :=
        [stateUpdate, ...instance.pendingStateUpdates^];
    | _ => ()
    },
  send: action =>
    switch instance.component {
    | Some(component) =>
      let stateUpdate = component.reducer(action);
      instance.pendingStateUpdates :=
        [stateUpdate, ...instance.pendingStateUpdates^];
    | _ => ()
    }
};

let createInstance = (~component, ~element, ~instanceSubTree, ~subElements) => {
  let iState = component.initialState();
  {
    component: Some(component),
    element,
    domElement: None,
    iState,
    instanceSubTree,
    subElements,
    pendingStateUpdates: ref([])
  };
};

let rec mapRenderedElement = (f, renderedElement) =>
  switch renderedElement {
  | IFlat(l) =>
    let nextL = List.map(f, l);
    let unchanged = List.for_all2((===), l, nextL);
    unchanged ? renderedElement : IFlat(nextL);
  | INested(s, l, d) =>
    let nextL = List.map(mapRenderedElement(f), l);
    let unchanged = List.for_all2((===), l, nextL);
    unchanged ? renderedElement : INested(s, nextL, d);
  };

let rec renderReactElement = reactElement : renderedElement =>
  switch reactElement {
  | Flat(l) => IFlat(List.map(reconcile, l))
  | Nested(name, props, elements) =>
    if (name == "List") {
      INested(name, List.map(renderReactElement, elements), None);
    } else {
      INested(name, List.map(renderReactElement, elements), None);
    }
  }
and reconcile = element : opaqueInstance =>
  switch element {
  | Component(component) =>
    let instance =
      createInstance(
        ~component,
        ~element,
        ~instanceSubTree=IFlat([]),
        ~subElements=Flat([])
      );
    let self = createSelf(~instance);
    let subElements = component.render(self);
    let instanceSubTree = renderReactElement(subElements);
    Instance({...instance, instanceSubTree, subElements});
  | String(value) =>
    Instance({
      component: None,
      element,
      iState: (),
      instanceSubTree: IFlat([]),
      domElement: None,
      subElements: Flat([]),
      pendingStateUpdates: ref([])
    })
  };

let executePendingStateUpdates = opaqueInstance => {
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
      executeUpdates(~state=nextState, otherStateUpdates);
    };
  let pendingUpdates = List.rev(instance.pendingStateUpdates^);
  instance.pendingStateUpdates := [];
  let nextState = executeUpdates(~state=instance.iState, pendingUpdates);
  instance.iState === nextState ?
    opaqueInstance : Instance({...instance, iState: nextState});
};

let flushPendingUpdatesFromInstance = instance => {
  let Instance({iState}) as instance = executePendingStateUpdates(instance);
  Js.log(iState);
  instance;
};

let flushPendingUpdates = renderedElement =>
  mapRenderedElement(flushPendingUpdatesFromInstance, renderedElement);

let rec renderRenderedElement =
        (
          parentElement: Dom.element,
          oldRenderedElement: option(renderedElement),
          newRenderedElement: renderedElement
        ) =>
  switch (oldRenderedElement, newRenderedElement) {
  | (Some(IFlat(oldList)), IFlat(newList))
      when List.length(oldList) == List.length(newList) =>
    IFlat(
      List.map2(
        (a, b) => reconcileRenderedElement(parentElement, Some(a), b),
        oldList,
        newList
      )
    )
  | (
      Some(INested(oldName, oldElements, Some(oldDom))),
      INested(newName, newElements, Some(newDom))
    )
      when oldName == newName =>
    if (newName == "List") {
      INested(
        newName,
        List.map2(
          (a, b) => renderRenderedElement(oldDom, Some(a), b),
          oldElements,
          newElements
        ),
        Some(oldDom)
      );
    } else {
      let node = Document.createElement(newName, document);
      Element.appendChild(node, oldDom);
      INested(
        newName,
        List.map2(
          (a, b) => renderRenderedElement(node, Some(a), b),
          oldElements,
          newElements
        ),
        Some(node)
      );
    }
  | (Some(oldRenderedElement), newRenderedElement) =>
    switch oldRenderedElement {
    | IFlat(l) =>
      List.iter(
        (Instance({domElement})) =>
          switch domElement {
          | Some(d) =>
            let _ = Element.removeChild(parentElement, d);
            ();
          | None => ()
          },
        l
      )
    | INested(_, _, Some(d)) =>
      let _ = Element.removeChild(parentElement, d);
      ();
    };
    switch newRenderedElement {
    | IFlat(l) =>
      IFlat(List.map(a => reconcileRenderedElement(parentElement, None, a), l))
    | INested(name, elements, dom) =>
      if (name == "List") {
        INested(
          name,
          List.map(
            x => renderRenderedElement(parentElement, None, x),
            elements
          ),
          dom
        );
      } else {
        let node = Document.createElement(name, document);
        Element.appendChild(node, parentElement);
        INested(
          name,
          List.map(renderRenderedElement(node, None), elements),
          Some(node)
        );
      }
    };
  | (None, v) => v
  }
and reconcileRenderedElement =
    (parentElement, oldOpaqueInstance, newOpaqueInstance) =>
  switch (oldOpaqueInstance, newOpaqueInstance) {
  | (
      Some(Instance({domElement: Some(oldDomElement), element: oldElement})),
      Instance({domElement: newDomElement, element: newElement} as newInstance)
    ) =>
    Element.removeChild(parentElement, oldDomElement);
    switch (oldElement, newElement) {
    | (Component(oldComponent), Component(newComponent))
        when oldComponent.debugName == newComponent.debugName =>
      let self = createSelf(~instance=newInstance);
      let subElements = newComponent.render(Obj.magic(self));
      let instanceSubTree = renderReactElement(subElements);
      Instance({
        ...newInstance,
        instanceSubTree,
        subElements,
        domElement: Some(oldDomElement)
      });
    | (String(oldValue), String(newValue)) =>
      Element.setInnerText(oldDomElement, newValue);
      Instance({...newInstance, element: newElement});
    };
  | (None, newInstance) =>
    let Instance({domElement: Some(domElement), element} as instance) = newInstance;
    Element.removeChild(parentElement, domElement);
    switch element {
    | Component(component) =>
      let self = createSelf(~instance);
      let subElements = component.render(Obj.magic(self));
      let instanceSubTree = renderReactElement(subElements);
      Instance({
        ...instance,
        instanceSubTree,
        subElements,
        domElement: Some(domElement)
      });
    | String(value) =>
      Element.setInnerText(domElement, value);
      Instance({...instance, element});
    };
  };

let rerender = (parentElement, renderedElement) => {
  let flushedRenderedElement = flushPendingUpdates(renderedElement);
  if (renderedElement === flushedRenderedElement) {
    renderedElement;
  } else {
    renderRenderedElement(
      parentElement,
      Some(renderedElement),
      flushedRenderedElement
    );
  };
};

let render = (reactElement, parentElement: Dom.element) => {
  let instance =
    switch (Element.lastElementChild(parentElement)) {
    | Some(childElement) =>
      let _ = Element.removeChild(childElement, parentElement);
      renderReactElement(reactElement);
    | None => renderReactElement(reactElement)
    };
  globalInstance := instance;
  let _ =
    Js.Global.setInterval(
      () => globalInstance := rerender(parentElement, globalInstance^),
      100
    );
  globalInstance;
};