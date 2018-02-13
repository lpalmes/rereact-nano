let render = () =>
  switch (
    Bs_webapi.Dom.Document.getElementById("container", Bs_webapi.Dom.document)
  ) {
  | Some(dom) =>
    let instance = RereactDom.render(<MiniTest />, dom);
    ();
  | None => print_endline("No dom element found :(")
  };

module ParcelModule = {
  [@bs.val] [@bs.scope "module.hot"]
  external dispose : (unit => unit) => unit = "";
  [@bs.val] [@bs.scope "module.hot"]
  external accept : (unit => unit) => unit = "";
};

render();