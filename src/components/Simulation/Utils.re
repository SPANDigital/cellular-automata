[@bs.val] external parseInt : (string, int) => int = "parseInt";

let binaryToRuleIndex = (l: int, m: int, r: int) : int => {
  let lstring = string_of_int(l);
  let mstring = string_of_int(m);
  let rstring = string_of_int(r);
  let str = {j|$lstring$mstring$rstring|j};
  parseInt(str, 2);
};

let rules =
    (
      ~left: int,
      ~middle: int,
      ~right: int,
      ~ruleset as rules: SimTypes.ruleset,
    ) => {
  let index = binaryToRuleIndex(left, middle, right);
  List.nth(List.rev(rules), index);
};

let cellGenerator = (simType, ~cellsPerRow: int=21, ()) =>
  SimTypes.(
    switch (simType) {
    | Simple => [|1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0|]
    | _ => Array.init(cellsPerRow, i => i === cellsPerRow / 2 ? 1 : 0)
    }
  );

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
      rowIndex > 0 && rowIndex < genMax - 1 ?
        Array.mapi(
          (colIndex, cell) =>
            colIndex > 0 && colIndex < cellsPerRow - 1 ?
              calculateNeighbours(cell, cells, rowIndex, colIndex)
              |> calculateCell(cell) :
              cell,
          row,
        ) :
        row,
    cells,
  );

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