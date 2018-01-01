open Rereact.ReactDom;

open Bs_webapi.Dom;

type appState = {
  frameworks: list(string),
  edit: bool
};

type action =
  | AddFramework(string)
  | Edit(bool);

let reducer = (state, action) =>
  switch action {
  | AddFramework(value) => {...state, frameworks: [value, ...state.frameworks]}
  | Edit(edit) => {...state, edit}
  };

let store = Reductive.Store.create(~reducer, ~preloadedState={frameworks: [], edit: false}, ());

module Test = {
  let component = Rereact.statelessComponent("Test");
  let createElement = (~name="Reason", ~children, _) =>
    Rereact.element({
      ...component,
      render: () => <h4> (Rereact.stringToElement(name)) (Rereact.listToElement(children)) </h4>
    });
};

module RecursiveComponent = {
  let component = Rereact.statelessComponent("Test");
  let rec createElement = (~count, ~children, _) =>
    Rereact.element({
      ...component,
      render: () => {
        Js.log(count);
        <div>
          (Rereact.stringToElement(count |> string_of_int))
          (count == 1 ? Rereact.nullElement : createElement(~count=count - 1, ~children, ()))
        </div>
      }
    });
};

module Sample = {
  let component = Rereact.statelessComponent("Sample");
  let createElement = (~name, ~frameworks, ~children, _) =>
    Rereact.element({
      ...component,
      render: () =>
        <div>
          <Test>
            <button onClick=((_) => Reductive.Store.dispatch(store, AddFramework("Rereact")))>
              (Rereact.stringToElement("click me"))
            </button>
            (Rereact.listToElement(List.map((name) => <Test name />, frameworks)))
          </Test>
          <input value="Hello my friend" onChange=((e) => Js.log(e)) />
        </div>
    });
};

let renderApp = () => {
  let state = Reductive.Store.getState(store);
  switch (Document.getElementById("container", document)) {
  | Some(dom) => render(<Sample name="ReasonML BA" frameworks=state.frameworks />, dom)
  | None => print_endline("No dom element found :(")
  }
};

renderApp();

Reductive.Store.subscribe(store, renderApp);