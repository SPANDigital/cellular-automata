let component = ReasonReact.statelessComponent("Stacking");

let make = (~cellWidth: int, ~cells: array(array(int)), _children) => {
  ...component,
  render: _self => {
    let cells =
      Array.mapi(
        (rowIndex, cellRow) =>
          <div key=(string_of_int(rowIndex) ++ "cellRow") className="cellRow">
            (
              ReasonReact.array(
                Array.mapi(
                  (index, cell) =>
                    <Cell
                      key=(
                        string_of_int(rowIndex)
                        ++ "-"
                        ++ string_of_int(index)
                        ++ "cell"
                      )
                      cell
                      cellSize=cellWidth
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