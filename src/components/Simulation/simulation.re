[%bs.raw {|require('./simulation.css')|}];

[@bs.val] external parseInt : (string, int) => int = "parseInt";

type cells = list(int);

type simData = {
  simple: cells,
  stacking: list(cells),
};

type simStates =
  | Playing
  | Paused
  | Stopped;

type simTypes =
  | Simple
  | Stacking;

type simpleRuleSet = list(int);

type timerId = ref(option(Js.Global.intervalId));

type state = {
  simData,
  storedData: ref(option(simData)),
  generation: int,
  status: simStates,
  simType: simTypes,
  timerId,
};

type action =
  | NextGeneration
  | Start
  | Stop
  | Pause
  | SwitchType(simTypes)
  | UpdateCells(int);

let component = ReasonReact.reducerComponent("Simulation");

let binaryToRuleIndex = (l: int, m: int, r: int) : int => {
  let lstring = string_of_int(l);
  let mstring = string_of_int(m);
  let rstring = string_of_int(r);
  let str = {j|$lstring$mstring$rstring|j};
  parseInt(str, 2);
};

let rules = (~left: int, ~middle: int, ~right: int) => {
  let index = binaryToRuleIndex(left, middle, right);
  let ruleset = [0, 1, 0, 1, 1, 0, 1, 0];
  List.nth(ruleset, index);
};

let rec buildList = (count, generator, acc) => {
  let list = [generator(List.length(acc)), ...acc];
  if (List.length(acc) === count) {
    acc;
  } else {
    buildList(count, generator, list);
  };
};

let cellGenerator = (simType, ~cellsPerRow: int=10, ()) =>
  switch (simType) {
  | Simple => [1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0]
  | Stacking =>
    buildList(
      cellsPerRow,
      count =>
        if (count === cellsPerRow / 2) {
          1;
        } else {
          0;
        },
      [],
    )
  };

let simTypeToString = (simType: simTypes) =>
  switch (simType) {
  | Simple => "Simple"
  | Stacking => "Wolfram Elementary"
  };

let stringToSimType = string =>
  switch (string) {
  | "Simple" => Simple
  | "Wolfram Elementary" => Stacking
  | _ => Simple
  };

let initialSimple = cellGenerator(Simple, ());

let initialStacking = [cellGenerator(Stacking, ~cellsPerRow=10, ())];

let clearTimer = state =>
  switch (state.timerId^) {
  | Some(id) =>
    Js.Global.clearInterval(id);
    state.timerId := None;
  | None => ()
  };

let make = _children => {
  ...component,
  initialState: () => {
    simData: {
      simple: initialSimple,
      stacking: initialStacking,
    },
    storedData: ref(None),
    generation: 1,
    status: Stopped,
    simType: Simple,
    timerId: ref(None),
  },
  reducer: (action, state: state) =>
    switch (action) {
    | SwitchType(simType) => ReasonReact.Update({...state, simType})
    | UpdateCells(_gen) =>
      switch (state.simType) {
      | Simple =>
        let currentCells = state.simData.simple;
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
        ReasonReact.Update({
          ...state,
          simData: {
            ...state.simData,
            simple: newCells,
          },
        });
      | Stacking => ReasonReact.NoUpdate
      }
    | NextGeneration =>
      ReasonReact.UpdateWithSideEffects(
        {...state, generation: state.generation + 1},
        (self => self.send(UpdateCells(state.generation))),
      )
    | Start =>
      if (state.status === Stopped) {
        state.storedData := Some(state.simData);
      };
      ReasonReact.UpdateWithSideEffects(
        {...state, status: Playing},
        (
          self => {
            state.timerId :=
              Some(
                Js.Global.setInterval(() => self.send(NextGeneration), 500),
              );
            self.onUnmount(() => clearTimer(state));
          }
        ),
      );
    | Stop =>
      let storedData =
        switch (state.storedData^) {
        | Some(data) => data
        | None => state.simData
        };
      ReasonReact.UpdateWithSideEffects(
        {...state, generation: 1, status: Stopped, simData: storedData},
        (_self => clearTimer(state)),
      );
    | Pause =>
      if (state.status === Playing) {
        ReasonReact.UpdateWithSideEffects(
          {...state, status: Paused},
          (_self => clearTimer(state)),
        );
      } else {
        ReasonReact.NoUpdate;
      }
    },
  render: self => {
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
          <select
            value=(simTypeToString(self.state.simType))
            onChange=(
              event =>
                self.send(
                  SwitchType(
                    stringToSimType(
                      ReactDOMRe.domElementToObj(
                        ReactEventRe.Form.target(event),
                      )##value,
                    ),
                  ),
                )
            )>
            <option> (ReasonReact.string("Simple")) </option>
            <option> (ReasonReact.string("Wolfram Elementary")) </option>
          </select>
        </div>
        (
          switch (self.state.simType) {
          | Simple => <Simple cells=self.state.simData.simple />
          | Stacking => <Stacking cells=self.state.simData.stacking />
          }
        )
      </div>
    </div>;
  },
};