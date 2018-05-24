[%bs.raw {|require('./simulation.css')|}];

[@bs.val] external parseInt : (string, int) => int = "parseInt";

type cells = list(int);

type stackingConfig = {
  genMax: int,
  cells: list(cells),
  cellHeight: int,
};

type simpleConfig = {
  genMax: int,
  cells,
  cellHeight: int,
};

type simData = {
  simple: simpleConfig,
  stacking: stackingConfig,
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
  | UpdateCells;

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

let rec buildList = (cellsPerRow, generator, acc) => {
  let list = [generator(List.length(acc)), ...acc];
  if (List.length(acc) === cellsPerRow) {
    acc;
  } else {
    buildList(cellsPerRow, generator, list);
  };
};

let cellGenerator = (simType, ~cellsPerRow: int=21, ()) =>
  switch (simType) {
  | Simple => [1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0]
  | Stacking =>
    buildList(
      cellsPerRow,
      count =>
        if (count === cellsPerRow / 2) {
          Js.log(cellsPerRow / 2);
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

let calculateNextGen = (cells, index, cell) =>
  if (index === 0 || index === List.length(cells) - 1) {
    cell;
  } else {
    let left = List.nth(cells, index - 1);
    let middle = cell;
    let right = List.nth(cells, index + 1);
    rules(~left, ~middle, ~right);
  };

let initialSimple = cellGenerator(Simple, ());

let initialStacking = [cellGenerator(Stacking, ~cellsPerRow=61, ())];

let clearTimer = state =>
  switch (state.timerId^) {
  | Some(id) =>
    Js.Global.clearInterval(id);
    state.timerId := None;
  | None => ()
  };

let getContainerHeight = state =>
  switch (state.simType) {
  | Simple => "auto"
  | Stacking =>
    let data = state.simData.stacking;
    string_of_int(data.genMax * data.cellHeight) ++ "px";
  };

let make = _children => {
  ...component,
  initialState: () => {
    simData: {
      simple: {
        genMax: 100,
        cells: initialSimple,
        cellHeight: 30,
      },
      stacking: {
        genMax: 29,
        cells: initialStacking,
        cellHeight: 10,
      },
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
    | UpdateCells =>
      switch (state.simType) {
      | Simple =>
        if (state.generation < state.simData.simple.genMax) {
          let currentCells = state.simData.simple.cells;
          let newCells =
            List.mapi(calculateNextGen(currentCells), currentCells);
          ReasonReact.Update({
            ...state,
            simData: {
              ...state.simData,
              simple: {
                ...state.simData.simple,
                cells: newCells,
              },
            },
          });
        } else {
          clearTimer(state);
          ReasonReact.NoUpdate;
        }
      | Stacking =>
        if (state.generation < state.simData.stacking.genMax) {
          let currentGeneration = List.hd(state.simData.stacking.cells);
          let nextGeneration =
            List.mapi(
              calculateNextGen(currentGeneration),
              currentGeneration,
            );
          let newData = [nextGeneration, ...state.simData.stacking.cells];
          ReasonReact.Update({
            ...state,
            simData: {
              ...state.simData,
              stacking: {
                ...state.simData.stacking,
                cells: newData,
              },
            },
          });
        } else {
          clearTimer(state);
          ReasonReact.NoUpdate;
        }
      }
    | NextGeneration =>
      ReasonReact.UpdateWithSideEffects(
        {...state, generation: state.generation + 1},
        (self => self.send(UpdateCells)),
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
        <div
          className="cellContainer"
          style=(
            ReactDOMRe.Style.make(~height=getContainerHeight(self.state), ())
          )>
          (
            switch (self.state.simType) {
            | Simple => <Simple cells=self.state.simData.simple.cells />
            | Stacking =>
              <Stacking cells=(List.rev(self.state.simData.stacking.cells)) />
            }
          )
        </div>
      </div>
    </div>;
  },
};