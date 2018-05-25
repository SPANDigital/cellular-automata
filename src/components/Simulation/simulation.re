[%bs.raw {|require('./simulation.css')|}];

[@bs.val] external parseInt : (string, int) => int = "parseInt";

type cells = list(int);

type stackingConfig = {
  genMax: int,
  cells: list(cells),
  cellWidth: int,
  cellsPerRow: int,
};

type simpleConfig = {
  genMax: int,
  cells,
  cellWidth: int,
  cellsPerRow: int,
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

type ruleset = list(int);

type rulesets = list((string, ruleset));

type timerId = ref(option(Js.Global.intervalId));

type state = {
  simData,
  storedData: ref(option(simData)),
  generation: int,
  status: simStates,
  simType: simTypes,
  timerId,
  containerWidth: int,
  rulesets,
  activeRuleset: string,
};

type action =
  | NextGeneration
  | Start
  | Stop
  | Pause
  | SwitchType(simTypes)
  | SwitchRuleset(string)
  | UpdateCells;

let component = ReasonReact.reducerComponent("Simulation");

let binaryToRuleIndex = (l: int, m: int, r: int) : int => {
  let lstring = string_of_int(l);
  let mstring = string_of_int(m);
  let rstring = string_of_int(r);
  let str = {j|$lstring$mstring$rstring|j};
  parseInt(str, 2);
};

let rules =
    (~left: int, ~middle: int, ~right: int, ~ruleset as rules: ruleset) => {
  let index = binaryToRuleIndex(left, middle, right);
  List.nth(List.rev(rules), index);
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

let calculateNextGen = (cells, ruleset, index, cell) =>
  if (index === 0 || index === List.length(cells) - 1) {
    cell;
  } else {
    let left = List.nth(cells, index - 1);
    let middle = cell;
    let right = List.nth(cells, index + 1);
    rules(~left, ~middle, ~right, ~ruleset);
  };

let clearTimer = ({timerId, _}) =>
  switch (timerId^) {
  | Some(id) =>
    Js.Global.clearInterval(id);
    timerId := None;
  | None => ()
  };

let getContainerHeight = ({simType, simData: data, _}) =>
  switch (simType) {
  | Simple => "auto"
  | Stacking =>
    let {stacking: {genMax, cellWidth, _}, _} = data;
    string_of_int(genMax * cellWidth) ++ "px";
  };

let getCellWidth = (~cellsPerRow as c: int, ~containerWidth as cw: int) =>
  cw / c;

let make = _children => {
  let stackingCellsPerRow = 61;
  let containerWidth = 800;
  let initialSimple = cellGenerator(Simple, ());
  let initialStacking = [
    cellGenerator(Stacking, ~cellsPerRow=stackingCellsPerRow, ()),
  ];
  {
    ...component,
    initialState: () => {
      simData: {
        simple: {
          genMax: 100,
          cellsPerRow: 20,
          cells: initialSimple,
          cellWidth: getCellWidth(~cellsPerRow=20, ~containerWidth),
        },
        stacking: {
          genMax: 29,
          cellsPerRow: stackingCellsPerRow,
          cells: initialStacking,
          cellWidth:
            getCellWidth(~cellsPerRow=stackingCellsPerRow, ~containerWidth),
        },
      },
      storedData: ref(None),
      generation: 1,
      status: Stopped,
      simType: Simple,
      timerId: ref(None),
      containerWidth,
      rulesets: [
        ("90", [0, 1, 0, 1, 1, 0, 1, 0]),
        ("222", [1, 1, 0, 1, 1, 1, 1, 0]),
      ],
      activeRuleset: "90",
    },
    reducer: (action, state: state) =>
      switch (action) {
      | SwitchRuleset(ruleno) =>
        ReasonReact.Update({...state, activeRuleset: ruleno})
      | SwitchType(simType) => ReasonReact.Update({...state, simType})
      | UpdateCells =>
        let ruleset = ListLabels.assoc(state.activeRuleset, state.rulesets);
        switch (state.simType) {
        | Simple =>
          if (state.generation < state.simData.simple.genMax) {
            let currentCells = state.simData.simple.cells;
            let newCells =
              List.mapi(
                calculateNextGen(currentCells, ruleset),
                currentCells,
              );
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
                calculateNextGen(currentGeneration, ruleset),
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
        };
      | NextGeneration =>
        ReasonReact.UpdateWithSideEffects(
          {...state, generation: state.generation + 1},
          (self => self.send(UpdateCells)),
        )
      | Start =>
        if (state.status === Stopped) {
          state.storedData := Some(state.simData);
        };
        if (state.status !== Playing) {
          ReasonReact.UpdateWithSideEffects(
            {...state, status: Playing},
            (
              self => {
                state.timerId :=
                  Some(
                    Js.Global.setInterval(
                      () => self.send(NextGeneration),
                      500,
                    ),
                  );
                self.onUnmount(() => clearTimer(state));
              }
            ),
          );
        } else {
          ReasonReact.NoUpdate;
        };
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
      let {generation, _} = self.state;
      <div className="Page1">
        <div
          className="container"
          style=(
            ReactDOMRe.Style.make(
              ~width=string_of_int(self.state.containerWidth) ++ "px",
              (),
            )
          )>
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
                  ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                  |> stringToSimType
                  |> ((simType: simTypes) => self.send(SwitchType(simType)))
              )>
              <option> (ReasonReact.string("Simple")) </option>
              <option> (ReasonReact.string("Wolfram Elementary")) </option>
            </select>
            <select
              value=self.state.activeRuleset
              onChange=(
                event =>
                  ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
                  |> (
                    (rulecode: string) => self.send(SwitchRuleset(rulecode))
                  )
              )>
              <option> (ReasonReact.string("90")) </option>
              <option> (ReasonReact.string("222")) </option>
            </select>
          </div>
          <div
            className="cellContainer"
            style=(
              ReactDOMRe.Style.make(
                ~height=getContainerHeight(self.state),
                (),
              )
            )>
            (
              switch (self.state.simType) {
              | Simple =>
                <Simple
                  cellWidth=self.state.simData.simple.cellWidth
                  cells=self.state.simData.simple.cells
                />
              | Stacking =>
                <Stacking
                  cellWidth=self.state.simData.stacking.cellWidth
                  cells=(List.rev(self.state.simData.stacking.cells))
                />
              }
            )
          </div>
        </div>
      </div>;
    },
  };
};