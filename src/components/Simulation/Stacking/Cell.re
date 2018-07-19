let component = ReasonReact.statelessComponent("Cell");

let make =
    (~cellSize: int, ~indexes: (int, int), ~className: string, _children) => {
  ...component,
  render: _self => {
    let width = string_of_int(cellSize) ++ "px";
    let (rowIndex, colIndex) = indexes;
    let topPos = string_of_int(cellSize * rowIndex) ++ "px";
    let leftPos = string_of_int(cellSize * colIndex) ++ "px";
    <div
      style=(
        ReactDOMRe.Style.make(
          ~height=width,
          ~width,
          ~transform="translate(" ++ leftPos ++ ", " ++ topPos ++ ")",
          (),
        )
      )
      className=("cell stacking " ++ className)
    />;
  },
};

let render = (~cellSize: int, ~indexes: (int, int), ~className: string) => {
  let width = string_of_int(cellSize) ++ "px";
  let (rowIndex, colIndex) = indexes;
  let topPos = string_of_int(cellSize * rowIndex) ++ "px";
  let leftPos = string_of_int(cellSize * colIndex) ++ "px";
  <div
    key=(string_of_int(rowIndex) ++ "-" ++ string_of_int(colIndex) ++ "cell")
    style=(
      ReactDOMRe.Style.make(
        ~height=width,
        ~width,
        ~transform="translate(" ++ leftPos ++ ", " ++ topPos ++ ")",
        (),
      )
    )
    className=("cell stacking " ++ className)
  />;
};