[%bs.raw {|require('./simulation.scss')|}];

[@bs.val] external parseInt : (string, int) => int = "parseInt";

type timerId = ref(option(Js.Global.intervalId));

type state = {
  simData: SimTypes.data,
  storedData: ref(option(SimTypes.data)),
  generation: int,
  status: SimTypes.playerStates,
  simType: SimTypes.types,
  timerId,
  containerWidth: int,
  containerHeight: int,
  cellsPerRow: int,
  genMax: int,
  cellWidth: int,
  rulesets: SimTypes.rulesets,
  golpatterns: SimTypes.golpatterns,
  activeRuleset: string,
  activePattern: string,
  activeGolPattern: string,
  wrapEdges: bool,
  cellState: bool,
  playerActions: array(SimTypes.playerAction),
};

type action =
  | NextGeneration
  | PlayerAction(SimTypes.playerAction)
  | SwitchType(SimTypes.types)
  | SwitchRuleset(string)
  | SwitchGolPattern(string)
  | ToggleWrapEdges
  | ToggleCellState
  | Init
  | UpdateCells;

let component = ReasonReact.reducerComponent("Simulation");

let make = _children => {
  /* -------------------------------------
     Internal Methods
     -------------------------------------- */
  let clearTimer = ({timerId, _}: state) =>
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
  let makeGolGrid = ({cellsPerRow, activeGolPattern, genMax, _}) => {
    let golConfig = ListLabels.assoc(activeGolPattern, SimTypes.Init.gol);
    Utils.buildGrid(~config=golConfig, ~cols=cellsPerRow, ~rows=genMax);
  };
  let makeConfigs = ({cellsPerRow, _} as state, ~genMax) => {
    let initialSimple = [|Utils.cellGenerator(Simple, ())|];
    let initialStacking = [|
      Utils.cellGenerator(Stacking, ~cellsPerRow, ()),
    |];
    let initialGrid = makeGolGrid({...state, genMax});
    [
      ("simple", Some(initialSimple)),
      ("stacking", Some(initialStacking)),
      ("gol", Some(initialGrid)),
    ];
  };
  /* -------------------------------------
      Component Record
     -------------------------------------- */
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
      rulesets: SimTypes.Init.rulesets,
      golpatterns: SimTypes.Init.gol,
      activeRuleset: "90",
      activePattern: "block",
      activeGolPattern: "glider",
      wrapEdges: false,
      cellState: false,
      playerActions: [|Stop, Start, Pause|],
    },
    /* -------------------------------------
       Lifecycle Methdos
       -------------------------------------- */
    didMount: self => self.send(Init),
    /* -------------------------------------
       Reducer
       -------------------------------------- */
    reducer: (action, state: state) =>
      switch (action) {
      | Init =>
        let {cellsPerRow, containerWidth, containerHeight, _} = state;
        let cellWidth = Utils.getCellWidth(~cellsPerRow, ~containerWidth);
        let genMax = containerHeight / cellWidth + 1;
        let config = makeConfigs(state, ~genMax);
        ReasonReact.Update({...state, cellWidth, genMax, simData: config});
      | ToggleWrapEdges =>
        ReasonReact.Update({...state, wrapEdges: ! state.wrapEdges})
      | ToggleCellState =>
        ReasonReact.Update({...state, cellState: ! state.cellState})
      | SwitchRuleset(ruleno) =>
        ReasonReact.Update({...state, activeRuleset: ruleno})
      | SwitchGolPattern(pattern) =>
        let currData = ListLabels.remove_assq("gol", state.simData);
        let golConfig = makeGolGrid({...state, activeGolPattern: pattern});
        let simData = [("gol", Some(golConfig)), ...currData];
        state.storedData := Some(simData);
        ReasonReact.Update({...state, activeGolPattern: pattern, simData});
      | SwitchType(simType) =>
        switch (simType) {
        | GOL => ReasonReact.Update({...state, simType, wrapEdges: false})
        | Simple
        | Stacking =>
          ReasonReact.Update({...state, simType, cellState: false})
        }
      | UpdateCells =>
        let simType = SimTypes.Helpers.typeToKey(state.simType);
        let {genMax, cellsPerRow, wrapEdges, generation, _} = state;
        let currentConfig = ListLabels.assoc(simType, state.simData);
        switch (currentConfig) {
        | Some(cells) =>
          let tempConfig = ref(None);
          Utils.(
            switch (state.simType) {
            | GOL =>
              let nextGen = calculateNextCycle(genMax, cellsPerRow, cells);
              tempConfig := Some(nextGen);
            | Simple
            | Stacking =>
              let ruleset =
                ListLabels.assoc(state.activeRuleset, state.rulesets);
              let lastItemIndex = Array.length(cells) - 1;
              let currentGeneration = cells[lastItemIndex];
              let nextGen =
                Array.mapi(
                  calculateNextGen(currentGeneration, ruleset, wrapEdges),
                  currentGeneration,
                );
              let newCells =
                simType === "simple" ?
                  [nextGen] :
                  (
                    generation > genMax ?
                      cells |> Array.to_list |> List.tl :
                      cells |> Array.to_list
                  )
                  @ [nextGen];
              tempConfig := Some(Array.of_list(newCells));
            }
          );
          let newConfig = tempConfig^;
          let newConfigTuple = (simType, newConfig);
          let currData = ListLabels.remove_assq(simType, state.simData);
          ReasonReact.Update({
            ...state,
            simData: [newConfigTuple, ...currData],
          });
        | None => ReasonReact.NoUpdate
        };
      | NextGeneration =>
        ReasonReact.UpdateWithSideEffects(
          {...state, generation: state.generation + 1},
          (self => self.send(UpdateCells)),
        )
      | PlayerAction(actionType) =>
        switch (actionType) {
        | Start =>
          if (state.status === Stopped) {
            state.storedData := Some(state.simData);
          };
          state.status !== Playing ?
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
            ) :
            ReasonReact.NoUpdate;
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
          state.status === Playing ?
            ReasonReact.UpdateWithSideEffects(
              {...state, status: Paused},
              (_self => clearTimer(state)),
            ) :
            ReasonReact.NoUpdate
        }
      },
    /* -------------------------------------
       Render
       -------------------------------------- */
    render: self => {
      let {
        generation,
        status,
        playerActions,
        activeRuleset,
        rulesets,
        simType,
        golpatterns,
        activeGolPattern,
        cellState,
        _,
      } =
        self.state;
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
            (
              playerActions
              |> Array.map(action => {
                   let label = SimTypes.playerActionToString(action);
                   <ControlButton
                     key=label
                     label
                     active=(status === SimTypes.playerActionToState(action))
                     action=(PlayerAction(action))
                     sendAction=self.send
                   />;
                 })
              |> ReasonReact.array
            )
            <span className="generation">
              (ReasonReact.string({j|Generation: $generation|j}))
            </span>
            <select
              value=(SimTypes.Helpers.typeToString(self.state.simType))
              onChange=SimTypes.(
                         event =>
                           ReactDOMRe.domElementToObj(
                             ReactEventRe.Form.target(event),
                           )##value
                           |> Helpers.stringToType
                           |> (
                             (simType: types) =>
                               self.send(SwitchType(simType))
                           )
                       )>
              <option> (ReasonReact.string("Simple")) </option>
              <option> (ReasonReact.string("Wolfram Elementary")) </option>
              <option> (ReasonReact.string("Game of Life")) </option>
            </select>
            (
              switch (simType) {
              | GOL =>
                <div className="stacking-controls">
                  <RulesSelect
                    activeRuleset=activeGolPattern
                    sendAction=self.send
                    action=(a => SwitchGolPattern(a))
                    rulesets=golpatterns
                  />
                  <label>
                    (ReasonReact.string("Cell Lifecycles"))
                    <input
                      _type="checkbox"
                      value="cellstate"
                      checked=self.state.cellState
                      onChange=(_event => self.send(ToggleCellState))
                    />
                  </label>
                </div>
              | Stacking =>
                <div className="stacking-controls">
                  <RulesSelect
                    activeRuleset
                    sendAction=self.send
                    action=(a => SwitchRuleset(a))
                    rulesets
                  />
                  <label>
                    (ReasonReact.string("Wrap Edges"))
                    <input
                      _type="checkbox"
                      value="wrapedges"
                      checked=self.state.wrapEdges
                      onChange=(_event => self.send(ToggleWrapEdges))
                    />
                  </label>
                </div>
              | _ => ReasonReact.null
              }
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
              let simType = SimTypes.Helpers.typeToKey(self.state.simType);
              let config = ListLabels.assoc(simType, self.state.simData);
              switch (config) {
              | Some(cells) =>
                switch (self.state.simType) {
                | Simple =>
                  <Simple
                    cellWidth=(self.state.containerWidth / 20)
                    cells=cells[0]
                  />
                | _ =>
                  <Stacking
                    cellWidth=self.state.cellWidth
                    cells
                    cellStatus=cellState
                  />
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