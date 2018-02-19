let instance = ref(RereactDom.(IFlat([])));

switch (Webapi.Dom.Document.getElementById("container", Webapi.Dom.document)) {
| Some(dom) => instance := RereactDom.render(<MiniTest />, dom)
| None => print_endline("No dom element found :(")
};

print_endline("Rerender");