let component = ReasonReact.statelessComponent("Simple");

let make = (~cellWidth: int, ~cells: array(int), _children) => {
  ...component,
  render: _self => {
    let width = string_of_int(cellWidth) ++ "px";
    let height = string_of_int(cellWidth) ++ "px";
    let cells =
      Array.mapi(
        (index, cell) =>
          <div
            key=(string_of_int(index))
            style=(
              ReactDOMRe.Style.make(~height, ~width, ~lineHeight=width, ())
            )
            className=("cell simple " ++ (cell === 1 ? "alive" : ""))>
            (ReasonReact.string(string_of_int(cell)))
          </div>,
        cells,
      );
    <div className="cellRow"> (ReasonReact.array(cells)) </div>;
  },
};