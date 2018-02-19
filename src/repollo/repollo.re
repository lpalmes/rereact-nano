open RereactElements;

open Bs_fetch;

exception Graphql_error(string);

type state =
  | Loading
  | Loaded(Js.t({.}))
  | Error(string);

type action =
  | SetLoading
  | SetData(Js.t({.}))
  | SetError(string);

let createElement = (~query, ~render: 'a => Rereact.reactElement, ~children as _, _) =>
  Rereact.element({
    debugName: "MiniTest",
    initialState: () => Loading,
    didMount: self =>
      fetchWithInit(
        "https://api.appetit.com.ar/graph",
        RequestInit.make(
          ~method_=Post,
          ~body=
            Js.Dict.fromList([
              ("query", Js.Json.string(query##query)),
              ("variables", query##variables)
            ])
            |> Js.Json.object_
            |> Js.Json.stringify
            |> BodyInit.make,
          ~headers=HeadersInit.makeWithArray([|("content-type", "application/json")|]),
          ()
        )
      )
      |> Js.Promise.then_(resp =>
           if (Response.ok(resp)) {
             Response.json(resp)
             |> Js.Promise.then_(data =>
                  switch (Js.Json.decodeObject(data)) {
                  | Some(obj) =>
                    let data = Js.Dict.unsafeGet(obj, "data") |> query##parse;
                    self.send(SetData(data));
                    Js.Promise.resolve();
                  | None =>
                    self.send(SetError("Response is not an object"));
                    Js.Promise.reject @@ Graphql_error("Response is not an object");
                  }
                );
           } else {
             self.send(SetError("Request failed"));
             Js.Promise.reject @@ Graphql_error("Request failed: " ++ Response.statusText(resp));
           }
         )
      |> ignore,
    reducer: (action: action, _state) =>
      switch action {
      | SetLoading => Rereact.Update(Loading)
      | SetData(data) => Rereact.Update(Loaded(data))
      | SetError(error) => Rereact.Update(Error(error))
      },
    render: ({state}) =>
      switch state {
      | Loading => <div> (Rereact.stringToElement("loading")) </div>
      | Loaded(data) => render(data)
      | Error(error) => <div> (Rereact.stringToElement(error)) </div>
      }
  });