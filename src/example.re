open RereactDom;

module Test = {
  let component = Rereact.statelessComponent("Test");
  let createElement = (~name="Reason", ~children, _) =>
    Rereact.element({
      ...component,
      render: () =>
        <div> <h4> (Rereact.stringToElement(name)) </h4> (Rereact.listToElement(children)) </div>
    });
};

module Sample = {
  let component = Rereact.statelessComponent("Sample");
  let createElement = (~name, ~children, _) =>
    Rereact.element({
      ...component,
      render: () =>
        <div>
          (
            Rereact.listToElement(
              List.map(
                (e) => <div> (Rereact.stringToElement(string_of_int(e))) </div>,
                [1, 2, 3, 4, 5]
              )
            )
          )
          <Test>
            <button onClick=((_) => print_endline("hello"))>
              (Rereact.stringToElement("click me"))
            </button>
          </Test>
          <input value="Hello my friend" onChange=((e) => Js.log(e)) />
        </div>
    });
};

let numberOfRenders = ref(0);

let render = () =>
  switch (Bs_webapi.Dom.Document.getElementById("container", Bs_webapi.Dom.document)) {
  | Some(dom) =>
    let instance = render(<Sample name="Test" />, dom);
    Js.log(instance)
  | None => print_endline("No dom element found :(")
  };

module ParcelModule = {
  [@bs.val] [@bs.scope "module.hot"] external dispose : (unit => unit) => unit = "";
  [@bs.val] [@bs.scope "module.hot"] external accept : (unit => unit) => unit = "";
};

render();

ParcelModule.dispose(
  () => {
    let newValue = numberOfRenders^ + 1;
    numberOfRenders := newValue
  }
);

ParcelModule.accept(
  () => {
    render();
    numberOfRenders := numberOfRenders^ + 1;
    Js.log(numberOfRenders)
  }
);