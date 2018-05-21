let component = ReasonReact.statelessComponent("Stacking");

let make = (~cells: list(list(int)), _children) => {
  ...component,
  render: _self => {
    let cells =
      List.mapi(
        (rowIndex, cellRow) =>
          <div key=(string_of_int(rowIndex) ++ "cellRow") className="cellRow">
            (
              ReasonReact.array(
                Array.of_list(
                  List.mapi(
                    (index, cell) =>
                      <div
                        key=(string_of_int(index) ++ "cell")
                        className=("cell " ++ (cell === 1 ? "alive" : ""))>
                        (ReasonReact.string(string_of_int(cell)))
                      </div>,
                    cellRow,
                  ),
                ),
              )
            )
          </div>,
        cells,
      );
    <div className="cellContainer">
      (ReasonReact.array(Array.of_list(cells)))
    </div>;
  },
};