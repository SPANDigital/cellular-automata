let component = ReasonReact.statelessComponent("Stacking");

let make = (~cellWidth: int, ~cells: array(array(int)), _children) => {
  ...component,
  render: _self => {
    let cells =
      Array.mapi(
        (rowIndex, cellRow) =>
          ReasonReact.array(
            Array.mapi(
              (colIndex, cell) =>
                cell === 1 ?
                  <Cell
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
                  ReasonReact.null,
              cellRow,
            ),
          ),
        cells,
      );
    ReasonReact.array(cells);
  },
};