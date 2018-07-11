type cells = array(array(int));

type config = option(cells);

type data = list((string, config));

type states =
  | Playing
  | Paused
  | Stopped;

type types =
  | Simple
  | Stacking
  | GOL;

type ruleset = list(int);

type rulesets = list((string, ruleset));

type golpattern = list((int, int));

type golpatterns = list((string, golpattern));

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