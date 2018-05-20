[%bs.raw {|require('./page1.css')|}];

type cells = list(int);

type simStates =
  | Playing
  | Paused
  | Stopped;

type simTypes =
  | Simple
  | Stacking;

type simpleRuleSet = list(int);

type state = {
  cells,
  storedCells: ref(option(cells)),
  generation: int,
  status: simStates,
  simType: simTypes,
  timerId: ref(option(Js.Global.intervalId)),
};

type action =
  | NextGeneration
  | Start
  | Stop
  | Pause
  | UpdateCells(int);

let component = ReasonReact.reducerComponent("Page1");

let stringifyCombination = (l: int, m: int, r: int) => {
  let lstring = string_of_int(l);
  let mstring = string_of_int(m);
  let rstring = string_of_int(r);
  {j|$lstring$mstring$rstring|j};
};

let rules = (~left: int, ~middle: int, ~right: int) => {
  let combination = stringifyCombination(left, middle, right);
  let ruleset = [0, 1, 0, 1, 1, 0, 1, 0];
  switch (combination) {
  | "111" => List.nth(ruleset, 0)
  | "110" => List.nth(ruleset, 1)
  | "101" => List.nth(ruleset, 2)
  | "100" => List.nth(ruleset, 3)
  | "011" => List.nth(ruleset, 4)
  | "010" => List.nth(ruleset, 5)
  | "001" => List.nth(ruleset, 6)
  | "000" => List.nth(ruleset, 7)
  | _ => 0
  };
};

let make = _children => {
  ...component,
  initialState: () => {
    cells: [1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0],
    storedCells: ref(None),
    generation: 1,
    status: Stopped,
    simType: Simple,
    timerId: ref(None),
  },
  reducer: (action, state: state) =>
    switch (action) {
    | UpdateCells(_gen) =>
      let currentCells = state.cells;
      let newCells =
        List.mapi(
          (index, cell) =>
            if (index === 0 || index === List.length(currentCells) - 1) {
              cell;
            } else {
              let left = List.nth(currentCells, index - 1);
              let middle = cell;
              let right = List.nth(currentCells, index + 1);
              rules(~left, ~middle, ~right);
            },
          currentCells,
        );
      ReasonReact.Update({...state, cells: newCells});
    | NextGeneration =>
      ReasonReact.UpdateWithSideEffects(
        {...state, generation: state.generation + 1},
        (self => self.send(UpdateCells(state.generation))),
      )
    | Start =>
      if (state.status === Stopped) {
        state.storedCells := Some(state.cells);
      };
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
      );
    | Stop =>
      let storedCells =
        switch (state.storedCells^) {
        | Some(cells) => cells
        | None => state.cells
        };
      ReasonReact.UpdateWithSideEffects(
        {...state, generation: 1, status: Stopped, cells: storedCells},
        (
          _self =>
            switch (state.timerId^) {
            | Some(id) =>
              Js.Global.clearInterval(id);
              state.timerId := None;
            | None => ()
            }
        ),
      );
    | Pause =>
      if (state.status === Playing) {
        ReasonReact.UpdateWithSideEffects(
          {...state, status: Paused},
          (
            _self =>
              switch (state.timerId^) {
              | Some(id) =>
                Js.Global.clearInterval(id);
                state.timerId := None;
              | None => ()
              }
          ),
        );
      } else {
        ReasonReact.NoUpdate;
      }
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
          <button
            className=(
              "navButton " ++ (self.state.status === Stopped ? "active" : "")
            )
            onClick=(_event => self.send(Stop))>
            (ReasonReact.string("Reset"))
          </button>
          <button
            className=(
              "navButton " ++ (self.state.status === Playing ? "active" : "")
            )
            onClick=(_event => self.send(Start))>
            (ReasonReact.string("Play"))
          </button>
          <button
            className=(
              "navButton " ++ (self.state.status === Paused ? "active" : "")
            )
            onClick=(_event => self.send(Pause))>
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