let component = ReasonReact.statelessComponent("Simple");

let make = (~cells: list(int), _children) => {
  ...component,
  render: _self => {
    let cells =
      List.mapi(
        (index, cell) =>
          <div
            key=(string_of_int(index))
            className=("cell " ++ (cell === 1 ? "alive" : ""))>
            (ReasonReact.string(string_of_int(cell)))
          </div>,
        cells,
      );
    <div className="cellRow">
      (ReasonReact.array(Array.of_list(cells)))
    </div>;
  },
};