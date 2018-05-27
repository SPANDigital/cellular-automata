let component = ReasonReact.statelessComponent("Stacking");

let make = (~cellWidth: int, ~cells: array(array(int)), _children) => {
  ...component,
  render: _self => {
    let width = string_of_int(cellWidth) ++ "px";
    let height = string_of_int(cellWidth) ++ "px";
    let cells =
      Array.mapi(
        (rowIndex, cellRow) =>
          <div key=(string_of_int(rowIndex) ++ "cellRow") className="cellRow">
            (
              ReasonReact.array(
                Array.mapi(
                  (index, cell) =>
                    <div
                      key=(string_of_int(index) ++ "cell")
                      style=(ReactDOMRe.Style.make(~height, ~width, ()))
                      className=(
                        "cell stacking " ++ (cell === 1 ? "alive" : "")
                      )
                    />,
                  cellRow,
                ),
              )
            )
          </div>,
        cells,
      );
    ReasonReact.array(cells);
  },
};