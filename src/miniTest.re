open RereactElements;

open Nice;

module Test = {
  let createElement = (~children as _, _) =>
    Rereact.element({
      debugName: "Test component",
      initialState: () => (),
      reducer: (_, _) => Rereact.NoUpdate,
      render: (_) => <h1> (Rereact.stringToElement("Hello")) </h1>
    });
};

type superState = int;

type action =
  | Increment
  | Decrement;

let createElement = (~children as _, _) =>
  Rereact.element({
    debugName: "MiniTest",
    initialState: () => 1,
    reducer: (action: action, state) =>
      switch action {
      | Increment => Rereact.Update(state + 1)
      | Decrement =>
        if (state >= 1) {
          Rereact.Update(state - 1)
        } else {
          Rereact.NoUpdate
        }
      },
    render: ({state, send}) =>
      <div>
        <button onClick=((_) => send(Increment))> (Rereact.stringToElement("Increment")) </button>
        <span> (Rereact.stringToElement(string_of_int(state))) </span>
        <button onClick=((_) => send(Decrement))> (Rereact.stringToElement("Decrement")) </button>
        (
          state == 2 ?
            <ul>
              <li> (Rereact.stringToElement("blue")) </li>
              <li> (Rereact.stringToElement("grey")) </li>
              <li> (Rereact.stringToElement("yellow")) </li>
              <li> (Rereact.stringToElement("green")) </li>
              <h1> (Rereact.stringToElement("Gera re puto, no me rompas las bolas")) </h1>
            </ul> :
            <h1> (Rereact.stringToElement("Halo")) </h1>
        )
      </div>
  });
/* (
     Rereact.arrayToElement(
       Array.map(
         v =>
           <div> (Rereact.stringToElement(v |> string_of_int)) </div>,
         Array.init(state, x => x)
       )
     )
   ) */