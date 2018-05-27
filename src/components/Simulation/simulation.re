[%bs.raw {|require('./simulation.css')|}];

[@bs.val] external parseInt : (string, int) => int = "parseInt";

type cells = array(int);

type config = {
  genMax: int,
  cells: array(cells),
  cellWidth: int,
  cellsPerRow: int,
};

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
  rulesets,
  activeRuleset: string,
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

let calculateNextCycle = ({genMax, cellsPerRow, cells, _}) => {
  Array.mapi((rowIndex, row) => {
    if (rowIndex > 0 && rowIndex < genMax - 1) {
      Array.mapi((colIndex, cell) => {
        if (colIndex > 0 && colIndex < cellsPerRow - 1) {
          let neighbours = ref(0);
          for (i in -1 to 1) {
            for (j in -1 to 1) {
              neighbours := neighbours^ + cells[rowIndex + i][colIndex + j];
            };
          };
          neighbours := neighbours^ - cell;
          if (cell === 1 && neighbours^ < 2) {
            0;
          } else if (cell === 1 && neighbours^ > 3) {
            0;
          } else if (cell === 0 && neighbours^ === 3) {
            1;
          } else {
            cell;
          }
        } else {
          cell;
        }
      }, row);
    } else {
      row;
    }
  }, cells);
};

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

let golInitState = [
  ("block", [
    (1, 1),
    (1, 2),
    (2, 1),
    (2, 2)
  ]),
  ("blinker", [
    (2, 3),
    (3, 3),
    (4, 3)
  ]),
  ("toad", [
    (5, 5),
    (5, 6),
    (5, 7),
    (4, 4),
    (4, 5),
    (4, 6)
  ]),    
  ("glider", [
    (3, 3),
    (4, 4),
    (4, 5),
    (3, 5),
    (2, 5)
  ])
];

let buildGrid = (~cols: int, ~rows: int) => {
  let grid = Array.make_matrix(rows, cols, 0);
  ListLabels.assoc("glider", golInitState) 
  |> List.iter(coord => {
    let (y, x) = coord;
    grid[y][x] = 1;
  });
  grid;
};

let make = _children => {
  let cellsPerRow = 60;
  let containerWidth = 800;
  let containerHeight = 400;
  let cellWidth = getCellWidth(~cellsPerRow, ~containerWidth);
  let genMax = containerHeight / cellWidth + 1;
  let initialSimple = [|cellGenerator(Simple, ())|];
  let initialStacking = [|cellGenerator(Stacking, ~cellsPerRow, ())|];
  let initialGrid = buildGrid(~cols=cellsPerRow, ~rows=genMax);
  {
    ...component,
    initialState: () => {
      simData: [
        (
          "simple",
          {
            genMax: 100,
            cellsPerRow: 20,
            cells: initialSimple,
            cellWidth: getCellWidth(~cellsPerRow=20, ~containerWidth),
          },
        ),
        (
          "stacking",
          {genMax, cellsPerRow, cells: initialStacking, cellWidth},
        ),
        ("gol", {genMax, cellsPerRow, cells: initialGrid, cellWidth}),
      ],
      storedData: ref(None),
      generation: 1,
      status: Stopped,
      simType: Simple,
      timerId: ref(None),
      containerWidth,
      containerHeight,
      rulesets: [
        ("90", [0, 1, 0, 1, 1, 0, 1, 0]),
        ("222", [1, 1, 0, 1, 1, 1, 1, 0]),
        ("190", [1, 0, 1, 1, 1, 1, 1, 0]),
        ("30", [0, 0, 0, 1, 1, 1, 1, 0]),
        ("110", [0, 1, 1, 0, 1, 1, 1, 0]),
      ],
      activeRuleset: "90",
      wrapEdges: false,
    },
    reducer: (action, state: state) =>
      switch (action) {
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
        let currentConfig = ListLabels.assoc(simType, state.simData);
        switch (state.simType) {
        | GOL =>
          let currentGeneration = currentConfig.cells;
          let nextGen = calculateNextCycle(currentConfig);
          let newConfig = {...currentConfig, cells: nextGen};
          let newConfigTuple = (simType, newConfig);
          let currData = ListLabels.remove_assq(simType, state.simData);
          ReasonReact.Update({...state, simData: [newConfigTuple, ...currData]});
        | Simple
        | Stacking =>
          let ruleset = ListLabels.assoc(state.activeRuleset, state.rulesets);
          let {wrapEdges, _} = state;
          let currentConfig = ListLabels.assoc(simType, state.simData);
          let lastItemIndex = Array.length(currentConfig.cells) - 1;
          let currentGeneration = currentConfig.cells[lastItemIndex];
          let nextGen =
            Array.mapi(
              calculateNextGen(currentGeneration, ruleset, wrapEdges),
              currentGeneration,
            );
          let newCells =
            switch (state.simType) {
            | Simple => [nextGen]
            | Stacking =>
              let currentCells =
                state.generation > currentConfig.genMax ?
                  currentConfig.cells |> Array.to_list |> List.tl :
                  currentConfig.cells |> Array.to_list;
              currentCells @ [nextGen];
            };
          let newConfig = {...currentConfig, cells: Array.of_list(newCells)};
          let newConfigTuple = (simType, newConfig);
          let currData = ListLabels.remove_assq(simType, state.simData);
          ReasonReact.Update({
            ...state,
            simData: [newConfigTuple, ...currData],
          });
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
            (
              switch (self.state.simType) {
              | Simple =>
                <Simple
                  cellWidth=ListLabels.assoc("simple", self.state.simData).
                              cellWidth
                  cells=ListLabels.assoc("simple", self.state.simData).cells[0]
                />
              | _ =>
                let simType = simTypeToKey(self.state.simType);
                let data = ListLabels.assoc(simType, self.state.simData);
                <Stacking cellWidth=data.cellWidth cells=data.cells />;
              }
            )
          </div>
        </div>
      </div>;
    },
  };
};