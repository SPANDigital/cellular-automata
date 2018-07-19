let component = ReasonReact.statelessComponent("Stacking");

let make =
    (
      ~cellWidth: int,
      ~cells: array(array(int)),
      ~cellStatus: bool=false,
      _children,
    ) => {
  ...component,
  render: _self => {
    let cells =
      Array.mapi(
        (rowIndex, cellRow) =>
          ReasonReact.array(
            Array.mapi(
              (colIndex, cell) =>
                cellStatus ?
                  <SmartCell
                    key=(
                      string_of_int(rowIndex)
                      ++ "-"
                      ++ string_of_int(colIndex)
                      ++ "cell"
                    )
                    indexes=(rowIndex, colIndex)
                    cell
                    cellSize=cellWidth
                  /> :
                  Cell.render(
                    ~cellSize=cellWidth,
                    ~indexes=(rowIndex, colIndex),
                    ~className=cell === 1 ? "alive" : "",
                  ),
              cellRow,
            ),
          ),
        cells,
      );
    ReasonReact.array(cells);
  },
};