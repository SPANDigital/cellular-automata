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
                        className=(
                          "cell stacking " ++ (cell === 1 ? "alive" : "")
                        )
                      />,
                    cellRow,
                  ),
                ),
              )
            )
          </div>,
        cells,
      );
    ReasonReact.array(Array.of_list(cells));
  },
};