[%bs.raw {|require('./page1.css')|}];

type cells = list(int);

type simStates =
  | Playing
  | Paused
  | Stopped;

type state = {
  cells,
  generation: int,
  status: simStates,
  timerId: ref(option(Js.Global.intervalId)),
};

type action =
  | NextGeneration
  | Start
  | Stop
  | UpdateCells(int);

let component = ReasonReact.reducerComponent("Page1");

let make = _children => {
  ...component,
  initialState: () => {
    cells: [0, 0, 0, 1, 0, 0, 0, 0],
    generation: 1,
    status: Stopped,
    timerId: ref(None),
  },
  reducer: (action, state: state) =>
    switch (action) {
    | UpdateCells(index) =>
      Js.log(string_of_int(index));
      ReasonReact.NoUpdate;
    | NextGeneration =>
      ReasonReact.Update({...state, generation: state.generation + 1})
    | Start =>
      ReasonReact.UpdateWithSideEffects(
        {...state, status: Playing},
        (
          self => {
            state.timerId :=
              Some(
                Js.Global.setInterval(() => self.send(NextGeneration), 500),
              );
            self.onUnmount(() =>
              switch (state.timerId^) {
              | Some(id) =>
                Js.Global.clearInterval(id);
                state.timerId := None;
              | None => ()
              }
            );
          }
        ),
      )
    | Stop =>
      ReasonReact.UpdateWithSideEffects(
        {...state, generation: 1, status: Stopped},
        (
          _self =>
            switch (state.timerId^) {
            | Some(id) =>
              Js.Global.clearInterval(id);
              state.timerId := None;
            | None => ()
            }
        ),
      )
    },
  render: self => {
    let cells =
      List.mapi(
        (index, cell) =>
          <div
            key=(string_of_int(index))
            className=("cell " ++ (cell === 1 ? "alive" : ""))>
            (ReasonReact.string(string_of_int(cell)))
          </div>,
        self.state.cells,
      );
    let {generation} = self.state;
    <div className="Page1">
      <div className="container simple">
        <div className="controls">
          <button className="navButton" onClick=(_event => self.send(Stop))>
            (ReasonReact.string("Reset"))
          </button>
          <button className="navButton" onClick=(_event => self.send(Start))>
            (ReasonReact.string("Play"))
          </button>
          <button className="navButton">
            (ReasonReact.string("Pause"))
          </button>
          <span className="generation">
            (ReasonReact.string({j|Generation: $generation|j}))
          </span>
        </div>
        <div className="cellContainer">
          (ReasonReact.array(Array.of_list(cells)))
        </div>
      </div>
    </div>;
  },
};