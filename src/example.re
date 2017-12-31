open Rereact.ReactDom;

open Bs_webapi.Dom;

module Test = {
  let component = Rereact.statelessComponent("Test");
  let createElement = (~name, ~children, _) =>
    Rereact.element({...component, render: () => <h4> (Rereact.stringToElement(name)) </h4>});
};

module Sample = {
  let component = Rereact.statelessComponent("Sample");
  let createElement = (~name, ~children, _) =>
    Rereact.element({
      ...component,
      render: () =>
        <div>
          <h1> (Rereact.stringToElement(name)) </h1>
          (
            Rereact.listToElement(
              List.map((name) => <Test name />, ["React", "Vue", "Preact", "ReReact ;)"])
            )
          )
        </div>
    });
};

switch (Document.getElementById("container", document)) {
| Some(dom) => render(<Sample name="ReasonML BA" />, dom)
| None => print_endline("No dom element found :(")
};