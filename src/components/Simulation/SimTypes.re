type cells = array(array(int));

type config = option(cells);

type data = list((string, config));

type playerStates =
  | Playing
  | Paused
  | Stopped;

type playerAction =
  | Start
  | Stop
  | Pause;

let playerActionToState = playerAction =>
  switch (playerAction) {
  | Start => Playing
  | Stop => Stopped
  | Pause => Paused
  };

let playerActionToString = playerAction =>
  switch (playerAction) {
  | Start => "Play"
  | Stop => "Reset"
  | Pause => "Pause"
  };

type types =
  | Simple
  | Stacking
  | GOL;

type ruleset = list(int);

type rulesets = list((string, ruleset));

type golpattern = list((int, int));

type golpatterns = list((string, golpattern));

module Helpers = {
  let typeToString = (simType: types) =>
    switch (simType) {
    | Simple => "Simple"
    | Stacking => "Wolfram Elementary"
    | GOL => "Game of Life"
    };
  let stringToType = string =>
    switch (string) {
    | "Simple" => Simple
    | "Wolfram Elementary" => Stacking
    | "Game of Life" => GOL
    | _ => Simple
    };
  let typeToKey = (simType: types) =>
    switch (simType) {
    | Simple => "simple"
    | Stacking => "stacking"
    | GOL => "gol"
    };
};

module Init = {
  let gol = [
    ("block", [(1, 1), (1, 2), (2, 1), (2, 2)]),
    ("blinker", [(2, 3), (3, 3), (4, 3)]),
    ("toad", [(5, 5), (5, 6), (5, 7), (4, 4), (4, 5), (4, 6)]),
    ("glider", [(3, 3), (4, 4), (4, 5), (3, 5), (2, 5)]),
  ];
  let rulesets = [
    ("90", [0, 1, 0, 1, 1, 0, 1, 0]),
    ("222", [1, 1, 0, 1, 1, 1, 1, 0]),
    ("190", [1, 0, 1, 1, 1, 1, 1, 0]),
    ("30", [0, 0, 0, 1, 1, 1, 1, 0]),
    ("110", [0, 1, 1, 0, 1, 1, 1, 0]),
  ];
};