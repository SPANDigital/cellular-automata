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

let component = ReasonReact.reducerComponent("SmartCell");

let make = (~cellSize: int, ~indexes: (int, int), ~cell: int, _children) => {
  ...component,
  initialState: () => {status: Dead, cell},
  reducer: (action, state: state) =>
    switch (action) {
    | UpdateStatus(status) => ReasonReact.Update({...state, status})
    },
  willReceiveProps: self =>
    if (self.state.cell !== cell) {
      let status = self.state.cell === 0 && cell === 1 ? Born : Dying;
      {status, cell};
    } else {
      let status = cell === 1 ? Alive : Dead;
      {...self.state, status};
    },
  didMount: self =>
    cell === 1 ?
      self.send(UpdateStatus(Alive)) : self.send(UpdateStatus(Dead)),
  shouldUpdate: ({oldSelf, newSelf}) =>
    oldSelf.state.status !== newSelf.state.status,
  render: ({state: {status}}) =>
    switch (status) {
    | Dead => ReasonReact.null
    | _ =>
      Cell.render(
        ~cellSize,
        ~indexes,
        ~className=cellStatusToClassName(status),
      )
    },
};