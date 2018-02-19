open Rereact;

type renderedElement =
  | IFlat(list(opaqueInstance))
  | INested(string, list(renderedElement), Dom.element)
and instance('state, 'action) = {
  mutable component: component('state, 'action),
  element,
  mutable iState: 'state,
  mutable instanceSubTree: renderedElement,
  mutable dom: Dom.element,
  pendingStateUpdates: ref(list('state => update('state, 'action)))
}
and opaqueInstance =
  | Instance(instance('state, 'action)): opaqueInstance;

let globalInstance = ref(IFlat([]));

let addProps = (domElement: Dom.element, props: RereactProps.props) => {
  switch props.id {
  | Some(value) => Webapi.Dom.Element.setAttribute("id", value, domElement)
  | None => ()
  };
  switch props.className {
  | Some(value) => Webapi.Dom.Element.setAttribute("class", value, domElement)
  | None => ()
  };
  switch props.value {
  | Some(value) => Webapi.Dom.Element.setAttribute("value", value, domElement)
  | None => ()
  };
  switch props.onClick {
  | Some(func) => Webapi.Dom.Element.addEventListener("click", (_) => func(), domElement)
  | None => ()
  };
  switch props.onChange {
  | Some(func) => Webapi.Dom.Element.addEventListener("change", (_) => func(), domElement)
  | None => ()
  };
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
  instance.iState = nextState;
};

let createInstance = (~component, ~element) => {
  let iState = component.initialState();
  {
    component,
    element,
    dom: Webapi.Dom.Document.createElement("span", Webapi.Dom.document),
    iState,
    instanceSubTree: IFlat([]),
    pendingStateUpdates: ref([])
  };
};

let mapListToOptionals = (b: list('b)) : list(option('b)) => List.map(a => Some(a), b);

let rec addOptionalElements = howMany =>
  if (howMany == 0) {
    [];
  } else {
    [None] @ addOptionalElements(howMany - 1);
  };

let equalizeList = (a, b) => {
  let aLength = List.length(a);
  let bLength = List.length(b);
  if (aLength == bLength) {
    (mapListToOptionals(a), mapListToOptionals(b));
  } else {
    let maxLength = max(aLength, bLength);
    if (aLength == maxLength) {
      (mapListToOptionals(a), mapListToOptionals(b) @ addOptionalElements(aLength - bLength));
    } else {
      (mapListToOptionals(a) @ addOptionalElements(bLength - aLength), mapListToOptionals(b));
    };
  };
};

let rec reconcile =
        (parentDom: Dom.element, instance: option(renderedElement), element: option(reactElement)) =>
  switch (instance, element) {
  | (None, Some(Nested(name, props, elements))) =>
    let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
    Webapi.Dom.Element.appendChild(node, parentDom);
    addProps(node, props);
    let els =
      List.map(reconcile(node, None), List.map(e => Some(e), elements))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(INested(name, els, node));
  | (Some(INested(_, _, dom)), None) =>
    Webapi.Dom.Element.removeChild(dom, parentDom);
    None;
  | (Some(INested(iName, iElements, dom)), Some(Nested(name, props, elements))) when iName != name =>
    Webapi.Dom.Element.removeChild(dom, parentDom);
    let node = Webapi.Dom.Document.createElement(name, Webapi.Dom.document);
    Webapi.Dom.Element.appendChild(node, parentDom);
    addProps(node, props);
    let els =
      List.map(reconcile(node, None), List.map(e => Some(e), elements))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(INested(name, els, node));
  | (Some(INested(iName, iElements, dom)), Some(Nested(name, props, elements))) when iName == name =>
    let (a, b) = equalizeList(iElements, elements);
    /* List.iter(Js.log, iElements);
       print_newline();
       List.iter(Js.log, elements); */
    if (List.length(a) != List.length(b)) {
      Js.log("Lists are different size");
      Js.log(List.length(a));
      Js.log(List.length(b));
    } else {
      ();
    };
    let els =
      List.map2(reconcile(dom), a, b)
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(INested(name, els, dom));
  | (None, Some(Flat(elements))) =>
    let els =
      elements
      |> List.map(reconcileElement(parentDom, None))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         );
    Some(IFlat(els));
  | (Some(IFlat(instances)), Some(Flat(elements))) =>
    let els =
      elements
      |> List.map2(reconcileElement(parentDom), List.map(i => Some(i), instances))
      |> List.fold_left(
           (instances, e) =>
             switch e {
             | Some(instance) => [instance, ...instances]
             | None => instances
             },
           []
         )
      |> List.rev;
    Some(IFlat(els));
  | _ => None
  }
and reconcileElement =
    (parentDom: Dom.element, instance: option(opaqueInstance), element)
    : option(opaqueInstance) =>
  switch (instance, element) {
  | (None, Component(component)) =>
    let instance = createInstance(~component, ~element);
    let self = createSelf(Obj.magic(instance));
    let subElements = component.render(Obj.magic(self));
    let instanceSubTree = reconcile(parentDom, None, Some(subElements));
    switch instanceSubTree {
    | Some(v) =>
      switch v {
      | INested(_, _, dom) => instance.dom = dom
      | _ => ()
      };
      instance.instanceSubTree = v;
    | None => ()
    };
    Some(Instance(instance));
  | (
      Some(Instance({element: Component(_), dom, instanceSubTree} as instance)),
      Component(newComponent)
    ) =>
    instance.component = Obj.magic(newComponent);
    let self = createSelf(Obj.magic(instance));
    let subElements = newComponent.render(Obj.magic(self));
    let instanceSubTree = reconcile(dom, Some(instanceSubTree), Some(subElements));
    switch instanceSubTree {
    | Some(v) =>
      switch v {
      | INested(_, _, dom) => instance.dom = dom
      | _ => ()
      };
      instance.instanceSubTree = v;
    | None => ()
    };
    Some(Instance({...instance, element}));
  | (None, String(value)) =>
    Webapi.Dom.Element.setInnerText(parentDom, value);
    Some(
      Instance({
        component: basicComponent("String"),
        element,
        iState: (),
        instanceSubTree: IFlat([]),
        dom: parentDom,
        pendingStateUpdates: ref([])
      })
    );
  | (Some(Instance({element: String(iValue), dom} as instc)), String(value)) =>
    if (iValue == value) {
      Some(Instance(instc));
    } else {
      Webapi.Dom.Element.setInnerText(dom, value);
      Some(Instance({...instc, element}));
    }
  | _ => None
  }
and createSelf = instance : self(_) => {
  state: instance.iState,
  reduce: (payloadToAction, payload) => {
    let action = payloadToAction(payload);
    let stateUpdate = instance.component.reducer(action);
    instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^];
    executePendingStateUpdates(Instance(instance));
    reconcileElement(instance.dom, Some(Instance(instance)), instance.element) |> ignore;
  },
  send: action => {
    let stateUpdate = instance.component.reducer(action);
    instance.pendingStateUpdates := [stateUpdate, ...instance.pendingStateUpdates^];
    executePendingStateUpdates(Instance(instance));
    Js.log(instance.component.debugName);
    reconcileElement(instance.dom, Some(Instance(instance)), instance.element) |> ignore;
  }
};

let parentContainer = ref(Webapi.Dom.Document.createElement("span", Webapi.Dom.document));

let rootInstance: ref(option(renderedElement)) = ref(None);

let render = (reactElement, parentDom: Dom.element) => {
  parentContainer := parentDom;
  rootInstance := reconcile(parentDom, rootInstance^, Some(reactElement));
  switch rootInstance^ {
  | Some(v) => v
  | None => IFlat([])
  };
};

let hotUpdate = (reactElement, instance) =>
  switch (reconcile(parentContainer^, Some(instance), Some(reactElement))) {
  | Some(v) => v
  | None => IFlat([])
  };