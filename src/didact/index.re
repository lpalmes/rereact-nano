open DidactElements;

switch (
  Bs_webapi.Dom.Document.getElementById("container", Bs_webapi.Dom.document)
) {
| Some(dom) =>
  Js.log(dom);
  let componentA = <h1> (Didact.stringToElement("Hola")) </h1>;
  let componentB =
    <div> <button> (Didact.stringToElement("click me")) </button> </div>;
  Didact.render(componentA, dom) |> ignore;
  Js.Global.setInterval(
    () => {
      Didact.render(componentB, dom) |> ignore;
      ();
    },
    2000
  )
  |> ignore;
| None => print_endline("No dom element found :(")
};