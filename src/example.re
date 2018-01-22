open Rereact.ReactDom;

module Test = {
  let component = Rereact.statelessComponent("Test");
  let createElement = (~name="Reason", ~children, _) =>
    Rereact.element({
      ...component,
      render: () => <h4> (Rereact.stringToElement(name)) (Rereact.listToElement(children)) </h4>
    });
};

module Sample = {
  let component = Rereact.statelessComponent("Sample");
  let createElement = (~name, ~children, _) =>
    Rereact.element({
      ...component,
      render: () =>
        <div>
          <Test>
            <button onClick=((_) => print_endline("hello"))>
              (Rereact.stringToElement("click me"))
            </button>
          </Test>
          <input value="Hello my friend" onChange=((e) => Js.log(e)) />
        </div>
    });
};

let render = () =>
  switch (Bs_webapi.Dom.Document.getElementById("container", Bs_webapi.Dom.document)) {
  | Some(dom) => render(<Sample name="Test" />, dom)
  | None => print_endline("No dom element found :(")
  };

module ParcelModule = {
  [@bs.val] [@bs.scope "module.hot"] external dispose : (unit => unit) => unit = "";
  [@bs.val] [@bs.scope "module.hot"] external accept : (unit => unit) => unit = "";
};

render();

ParcelModule.accept(render);