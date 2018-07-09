[%bs.raw {|require('./simulation.css')|}];

[@bs.val] external parseInt : (string, int) => int = "parseInt";

type cells = array(array(int));

type config = option(cells);

type simData = list((string, config));

type simStates =
  | Playing
  | Paused
  | Stopped;

type simTypes =
  | Simple
  | Stacking
  | GOL;

type ruleset = list(int);

type rulesets = list((string, ruleset));

type golpattern = list((int, int));

type golpatterns = list((string, golpattern));

type timerId = ref(option(Js.Global.intervalId));

type state = {
  simData,
  storedData: ref(option(simData)),
  generation: int,
  status: simStates,
  simType: simTypes,
  timerId,
  containerWidth: int,
  containerHeight: int,
  cellsPerRow: int,
  genMax: int,
  cellWidth: int,
  rulesets,
  golpatterns,
  activeRuleset: string,
  activePattern: string,
  wrapEdges: bool,
};

type action =
  | NextGeneration
  | Start
  | Stop
  | Pause
  | SwitchType(simTypes)
  | SwitchRuleset(string)
  | ToggleWrapEdges
  | Init
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

let cellGenerator = (simType, ~cellsPerRow: int=21, ()) =>
  switch (simType) {
  | Simple => [|1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0|]
  | _ => Array.init(cellsPerRow, i => i === cellsPerRow / 2 ? 1 : 0)
  };

let simTypeToString = (simType: simTypes) =>
  switch (simType) {
  | Simple => "Simple"
  | Stacking => "Wolfram Elementary"
  | GOL => "Game of Life"
  };

let stringToSimType = string =>
  switch (string) {
  | "Simple" => Simple
  | "Wolfram Elementary" => Stacking
  | "Game of Life" => GOL
  | _ => Simple
  };

let simTypeToKey = (simType: simTypes) =>
  switch (simType) {
  | Simple => "simple"
  | Stacking => "stacking"
  | GOL => "gol"
  };

let calculateNextGen = (cells, ruleset, wrapEdges, index, cell) => {
  let atFirstIndex = index === 0;
  let atLastIndex = index === Array.length(cells) - 1;
  let atFirstOrLastIndex = atFirstIndex || atLastIndex;
  if (! wrapEdges && atFirstOrLastIndex) {
    cell;
  } else {
    let lIndex = atFirstIndex ? 7 : index - 1;
    let rIndex = atLastIndex ? 0 : index + 1;
    let left = cells[lIndex];
    let middle = cell;
    let right = cells[rIndex];
    rules(~left, ~middle, ~right, ~ruleset);
  };
};

let calculateNeighbours = (cell, cells, r, c) => {
  let neighbours = ref(0);
  for (i in (-1) to 1) {
    for (j in (-1) to 1) {
      neighbours := neighbours^ + cells[r + i][c + j];
    };
  };
  neighbours^ - cell;
};

let calculateCell = (cell, neighbours) =>
  if (cell === 1 && neighbours < 2) {
    0;
  } else if (cell === 1 && neighbours > 3) {
    0;
  } else if (cell === 0 && neighbours === 3) {
    1;
  } else {
    cell;
  };

let calculateNextCycle = (genMax, cellsPerRow, cells) =>
  Array.mapi(
    (rowIndex, row) =>
      switch (rowIndex > 0 && rowIndex < genMax - 1) {
      | true =>
        Array.mapi(
          (colIndex, cell) =>
            switch (colIndex > 0 && colIndex < cellsPerRow - 1) {
            | true =>
              calculateNeighbours(cell, cells, rowIndex, colIndex)
              |> calculateCell(cell)
            | _ => cell
            },
          row,
        )
      | _ => row
      },
    cells,
  );

let clearTimer = ({timerId, _}) =>
  switch (timerId^) {
  | Some(id) =>
    Js.Global.clearInterval(id);
    timerId := None;
  | None => ()
  };

let getContainerHeight = ({simType, containerHeight, _}) =>
  switch (simType) {
  | Simple => "auto"
  | _ => string_of_int(containerHeight) ++ "px"
  };

let getCellWidth = (~cellsPerRow as c: int, ~containerWidth as cw: int) =>
  cw / c;

let buildGrid = (~config: list((int, int)), ~cols: int, ~rows: int) => {
  let grid = Array.make_matrix(rows, cols, 0);
  config
  |> List.iter(coord => {
       let (y, x) = coord;
       grid[y][x] = 1;
     });
  grid;
};

let make = _children => {
  let golInitState = [
    ("block", [(1, 1), (1, 2), (2, 1), (2, 2)]),
    ("blinker", [(2, 3), (3, 3), (4, 3)]),
    ("toad", [(5, 5), (5, 6), (5, 7), (4, 4), (4, 5), (4, 6)]),
    ("glider", [(3, 3), (4, 4), (4, 5), (3, 5), (2, 5)]),
  ];
  {
    ...component,
    initialState: () => {
      simData: [("simple", None), ("stacking", None), ("gol", None)],
      storedData: ref(None),
      generation: 1,
      status: Stopped,
      simType: Simple,
      timerId: ref(None),
      containerWidth: 800,
      containerHeight: 400,
      cellsPerRow: 60,
      genMax: 0,
      cellWidth: 0,
      rulesets: [
        ("90", [0, 1, 0, 1, 1, 0, 1, 0]),
        ("222", [1, 1, 0, 1, 1, 1, 1, 0]),
        ("190", [1, 0, 1, 1, 1, 1, 1, 0]),
        ("30", [0, 0, 0, 1, 1, 1, 1, 0]),
        ("110", [0, 1, 1, 0, 1, 1, 1, 0]),
      ],
      golpatterns: [
        ("block", [(1, 1), (1, 2), (2, 1), (2, 2)]),
        ("blinker", [(2, 3), (3, 3), (4, 3)]),
        ("toad", [(5, 5), (5, 6), (5, 7), (4, 4), (4, 5), (4, 6)]),
        ("glider", [(3, 3), (4, 4), (4, 5), (3, 5), (2, 5)]),
      ],
      activeRuleset: "90",
      activePattern: "block",
      wrapEdges: false,
    },
    didMount: self => self.send(Init),
    reducer: (action, state: state) =>
      switch (action) {
      | Init =>
        let {cellsPerRow, containerWidth, containerHeight, _} = state;
        let cellWidth = getCellWidth(~cellsPerRow, ~containerWidth);
        let genMax = containerHeight / cellWidth + 1;
        let initialSimple = [|cellGenerator(Simple, ())|];
        let initialStacking = [|cellGenerator(Stacking, ~cellsPerRow, ())|];
        let golConfig = ListLabels.assoc("glider", golInitState);
        let initialGrid =
          buildGrid(~config=golConfig, ~cols=cellsPerRow, ~rows=genMax);
        let initialSimData = [
          ("simple", Some(initialSimple)),
          ("stacking", Some(initialStacking)),
          ("gol", Some(initialGrid)),
        ];
        ReasonReact.Update({
          ...state,
          cellWidth,
          genMax,
          simData: initialSimData,
        });
      | ToggleWrapEdges =>
        ReasonReact.Update({...state, wrapEdges: ! state.wrapEdges})
      | SwitchRuleset(ruleno) =>
        ReasonReact.Update({...state, activeRuleset: ruleno})
      | SwitchType(simType) => ReasonReact.Update({...state, simType})
      | UpdateCells =>
        let simType =
          switch (state.simType) {
          | Simple => "simple"
          | Stacking => "stacking"
          | GOL => "gol"
          };
        let {genMax, cellsPerRow, _} = state;
        let currentConfig = ListLabels.assoc(simType, state.simData);
        switch (currentConfig) {
        | Some(cells) =>
          switch (state.simType) {
          | GOL =>
            let nextGen = calculateNextCycle(genMax, cellsPerRow, cells);
            let newConfigTuple = (simType, Some(nextGen));
            let currData = ListLabels.remove_assq(simType, state.simData);
            ReasonReact.Update({
              ...state,
              simData: [newConfigTuple, ...currData],
            });
          | Simple
          | Stacking =>
            let ruleset =
              ListLabels.assoc(state.activeRuleset, state.rulesets);
            let {wrapEdges, generation, _} = state;
            let lastItemIndex = Array.length(cells) - 1;
            let currentGeneration = cells[lastItemIndex];
            let nextGen =
              Array.mapi(
                calculateNextGen(currentGeneration, ruleset, wrapEdges),
                currentGeneration,
              );
            let newCells =
              switch (state.simType) {
              | Simple => [nextGen]
              | _ =>
                let currentCells =
                  generation > genMax ?
                    cells |> Array.to_list |> List.tl : cells |> Array.to_list;
                currentCells @ [nextGen];
              };
            let newConfigTuple = (simType, Some(Array.of_list(newCells)));
            let currData = ListLabels.remove_assq(simType, state.simData);
            ReasonReact.Update({
              ...state,
              simData: [newConfigTuple, ...currData],
            });
          }
        | None => ReasonReact.NoUpdate
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
                      100,
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
              <option> (ReasonReact.string("Game of Life")) </option>
            </select>
            (
              self.state.simType === Stacking ?
                <div>
                  <select
                    value=self.state.activeRuleset
                    onChange=(
                      event =>
                        ReactDOMRe.domElementToObj(
                          ReactEventRe.Form.target(event),
                        )##value
                        |> (
                          (rulecode: string) =>
                            self.send(SwitchRuleset(rulecode))
                        )
                    )>
                    (
                      self.state.rulesets
                      |> List.map(rule => {
                           let (rulecode, _) = rule;
                           <option key=rulecode>
                             (ReasonReact.string(rulecode))
                           </option>;
                         })
                      |> Array.of_list
                      |> ReasonReact.array
                    )
                  </select>
                  <input
                    _type="checkbox"
                    value="wrapedges"
                    checked=self.state.wrapEdges
                    onChange=(_event => self.send(ToggleWrapEdges))
                  />
                </div> :
                ReasonReact.null
            )
          </div>
          <div
            className="cellContainer"
            style=(
              ReactDOMRe.Style.make(
                ~height=getContainerHeight(self.state),
                (),
              )
            )>
            {
              let simType = simTypeToKey(self.state.simType);
              let config = ListLabels.assoc(simType, self.state.simData);
              switch (config) {
              | Some(cells) =>
                switch (self.state.simType) {
                | Simple =>
                  <Simple
                    cellWidth=(self.state.containerWidth / 20)
                    cells=cells[0]
                  />
                | _ => <Stacking cellWidth=self.state.cellWidth cells />
                }
              | None => ReasonReact.null
              };
            }
          </div>
        </div>
      </div>;
    },
  };
};