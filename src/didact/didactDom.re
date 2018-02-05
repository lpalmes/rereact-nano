open Didact;

type nodeInstance = {
  element: didactElement,
  dom: Dom.element,
  childInstances: list(didactInstance)
}
and instance('state, 'action) = {
  component: component('state, 'action),
  iState: 'state,
  element: didactElement,
  childInstance: ref(option(didactInstance)),
  dom: ref(Dom.element),
  pendingStateUpdates: ref(list('state => update('state, 'action)))
}
and componentInstance =
  | Instance(instance('state, 'action)): componentInstance
and didactInstance =
  | NodeInstance(nodeInstance)
  | ComponentInstance(componentInstance);

module Reconciler = {
  let addProps = (domElement: Dom.element, props) => {
    open Bs_webapi.Dom;
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
  let equalizeLists = (aList, bList) : (list(option('a)), list(option('b))) => {
    let aOptionList = List.map(a => Some(a), aList);
    let bOptionList = List.map(a => Some(a), bList);
    (aOptionList, bOptionList);
  };
  let domFromInstance = newInstance =>
    switch newInstance {
    | NodeInstance({dom}) => dom
    | ComponentInstance(Instance({dom})) => dom^
    };
  let rec instantiate = (element: didactElement) : didactInstance => {
    open Bs_webapi.Dom;
    let instance =
      switch element.elementType {
      | Node(name) =>
        let node = Document.createElement(name, document);
        let childInstances = List.map(instantiate, element.children);
        List.iter(
          e =>
            switch e {
            | NodeInstance({dom}) =>
              let _ = Element.appendChild(dom, node);
              ();
            | ComponentInstance(Instance({dom})) =>
              let _ = Element.appendChild(dom^, node);
              ();
            },
          childInstances
        );
        NodeInstance({dom: node, element, childInstances});
      | Text(value) =>
        let dom = Document.createElement("span", document);
        Element.setInnerText(dom, value);
        NodeInstance({dom, element, childInstances: []});
      | Component(component) =>
        let instance = createInstance(Obj.magic(component), element);
        let self = createSelf(instance);
        let element = component.render(Obj.magic(self));
        let newInstance = instantiate(element);
        let Instance({dom, childInstance}) = instance;
        dom := domFromInstance(newInstance);
        childInstance := Some(newInstance);
        ComponentInstance(instance);
      };
    switch instance {
    | NodeInstance({dom}) => addProps(dom, element.props)
    | _ => ()
    };
    instance;
  }
  and reconcile =
      (
        parentDom: Dom.element,
        instance: option(didactInstance),
        didactElement: option(didactElement)
      ) => {
    let rec reconcilerImpl =
            (
              parentDom: Dom.element,
              instance: option(didactInstance),
              didactElement: option(didactElement)
            ) =>
      Bs_webapi.Dom.(
        switch (instance, didactElement) {
        | (None, Some(didactElement)) =>
          let newInstance = instantiate(didactElement);
          Element.appendChild(domFromInstance(newInstance), parentDom);
          Some(newInstance);
        | (Some(instance), None) =>
          Element.removeChild(domFromInstance(instance), parentDom) |> ignore;
          None;
        /* | (Some(NodeInstance({element} as instance)), Some(didactElement))
             when element.elementType == didactElement.elementType =>
           Some(NodeInstance({...instance, element: didactElement})) */
        | (
            Some(ComponentInstance(instance)),
            Some({elementType: Component(component)})
          ) =>
          let self = createSelf(Obj.magic(instance));
          let element = component.render(Obj.magic(self));
          let Instance({dom, childInstance}) = instance;
          childInstance :=
            reconcilerImpl(parentDom, childInstance^, Some(element));
          switch childInstance^ {
          | Some(c) => dom := domFromInstance(c)
          | _ => ()
          };
          Some(ComponentInstance(instance));
        | (Some(instance), Some(didactElement)) =>
          let newInstance = instantiate(didactElement);
          Element.removeChild(domFromInstance(instance), parentDom) |> ignore;
          Element.appendChild(domFromInstance(newInstance), parentDom);
          Some(newInstance);
        | (None, None) => None
        }
      );
    reconcilerImpl(parentDom, instance, didactElement);
  }
  and reconcileChildren =
      (instance: nodeInstance, didactElement: didactElement)
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
  }
  and createSelf = instance : self(_) => {
    let Instance(instance) = instance;
    Bs_webapi.Dom.{
      state: Obj.magic(instance.iState),
      reduce: (payloadToAction, payload) => {
        let action = payloadToAction(payload);
        let stateUpdate = instance.component.reducer(Obj.magic(action));
        instance.pendingStateUpdates :=
          [stateUpdate, ...instance.pendingStateUpdates^];
      },
      send: action => {
        let stateUpdate =
          switch (
            instance.component.reducer(Obj.magic(action), instance.iState)
          ) {
          | NoUpdate => instance.iState
          | Update(newState) => newState
          };
        switch (Element.parentNode(instance.dom^)) {
        | Some(dom) =>
          reconcile(
            Obj.magic(dom),
            Some(
              ComponentInstance(Instance({...instance, iState: stateUpdate}))
            ),
            Some(instance.element)
          )
          |> ignore
        | None => ()
        };
      }
    };
  }
  and createInstance = (component, element) : componentInstance => {
    let iState = component.initialState();
    Instance({
      component,
      dom:
        ref(
          Bs_webapi.Dom.Document.createElement("span", Bs_webapi.Dom.document)
        ),
      element,
      childInstance: ref(None),
      iState,
      pendingStateUpdates: ref([])
    });
  };
};

let instance: ref(option(didactInstance)) = ref(None);

let render = (element, parentDom) => {
  let newInstance = Reconciler.reconcile(parentDom, instance^, Some(element));
  instance := newInstance;
  newInstance;
};