open RereactDom;

type superState = int;

type action =
  | Increment(int)
  | Decrement(int);

let createElement = (~children as _, _) =>
  Rereact.element({
    debugName: "MiniTest",
    initialState: () => 1,
    reducer: (action: action, state) =>
      switch action {
      | Increment(value) => Rereact.Update(state + value)
      | Decrement(value) =>
        if (state - value >= 0) {
          Rereact.Update(state - value);
        } else {
          Rereact.NoUpdate;
        }
      },
    render: ({state, send}) =>
      <div>
        <button onClick=((_) => send(Increment(1)))>
          (Rereact.stringToElement("Increment"))
        </button>
        <span> (Rereact.stringToElement(string_of_int(state))) </span>
        <button onClick=((_) => send(Decrement(1)))>
          (Rereact.stringToElement("Decrement"))
        </button>
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