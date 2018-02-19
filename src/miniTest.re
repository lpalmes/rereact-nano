open RereactElements;

module VendorsQuery = [%graphql
  {|
    {
        vendors {
          id
          name
        }
      }
      |}
];

let vendorQuery = VendorsQuery.make();

let createElement = (~children as _, _) =>
  Rereact.element({
    debugName: "MiniTest",
    initialState: () => (),
    didMount: _self => (),
    reducer: (_, _) => Rereact.NoUpdate,
    render: (_) =>
      <Repollo
        query=vendorQuery
        render=(
          props => {
            Js.log(props);
            <div> (Rereact.stringToElement("Hello")) </div>;
          }
        )
      />
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