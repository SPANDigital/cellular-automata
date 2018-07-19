type cellStatus =
  | Dead
  | Alive
  | Born
  | Dying;

let cellStatusToClassName = cellStatus =>
  switch (cellStatus) {
  | Dead => "dead"
  | Alive => "alive"
  | Born => "born"
  | Dying => "dying"
  };

type state = {
  status: cellStatus,
  cell: int,
};

type actions =
  | UpdateStatus(cellStatus);

let component = ReasonReact.reducerComponent("Cell");

let make = (~cellSize: int, ~cell: int, _children) => {
  ...component,
  initialState: () => {status: Dead, cell},
  reducer: (action, state: state) =>
    switch (action) {
    | UpdateStatus(status) => ReasonReact.Update({...state, status})
    },
  willReceiveProps: self => {
    let prevCell = string_of_int(self.state.cell);
    let nextCell = string_of_int(cell);
    let str = {j| $prevCell - $nextCell |j};
    if (prevCell !== nextCell) {
      {...self.state, cell};
    } else {
      self.state;
    };
  },
  shouldUpdate: ({oldSelf, newSelf}) => oldSelf.state.cell !== cell,
  render: _self => {
    let width = string_of_int(cellSize) ++ "px";
    <div
      style=(ReactDOMRe.Style.make(~height=width, ~width, ()))
      className=("cell stacking " ++ (cell === 1 ? "alive" : ""))
    />;
  },
};