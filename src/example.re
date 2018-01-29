open RereactDom;

module MiniTest = {
  type superState = int;
  type action =
    | Increment(int)
    | Decrement(int);
  let createElement = (~children as _, _) =>
    Rereact.element({
      debugName: "Sample",
      initialState: () => 1,
      reducer: (action: action, state) =>
        switch action {
        | Increment(value) => Rereact.Update(state + value)
        | Decrement(value) => Rereact.Update(state - value)
        },
      render: ({state, send}) =>
        <div>
          <button onClick=((_) => send(Increment(10)))>
            (Rereact.stringToElement("Increment"))
          </button>
          <span> (Rereact.stringToElement(string_of_int(state))) </span>
        </div>
    });
};

module Sample = {
  type superState = {elements: list(int)};
  type action =
    | Add(int)
    | Remove(int)
    | Empty;
  let createElement = (~children as _, _) =>
    Rereact.element({
      debugName: "Sample",
      initialState: () => {elements: [1, 2, 4, 5]},
      reducer: (action: action, state) =>
        switch action {
        | Add(value) => Rereact.Update({elements: [value, ...state.elements]})
        | Remove(value) => Rereact.Update({elements: [value, ...state.elements]})
        | Empty => Rereact.Update({elements: []})
        },
      render: ({state, send}) =>
        <div>
          <MiniTest />
          <button onClick=((_) => send(Add(10)))> (Rereact.stringToElement("click me")) </button>
          (
            Rereact.listToElement(
              List.map(
                (e) => <div> (Rereact.stringToElement(string_of_int(e))) </div>,
                state.elements
              )
            )
          )
        </div>
    });
};

let render = () =>
  switch (Bs_webapi.Dom.Document.getElementById("container", Bs_webapi.Dom.document)) {
  | Some(dom) =>
    let instance = render(<Sample />, dom);
    Js.log(instance)
  | None => print_endline("No dom element found :(")
  };

module ParcelModule = {
  [@bs.val] [@bs.scope "module.hot"] external dispose : (unit => unit) => unit = "";
  [@bs.val] [@bs.scope "module.hot"] external accept : (unit => unit) => unit = "";
};

render();

ParcelModule.accept(render);