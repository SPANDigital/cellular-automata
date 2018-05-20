[%bs.raw {|require('./page1.css')|}];

type cells = list(int);

type state = {cells};

type action =
  | UpdateCell(int);

let component = ReasonReact.reducerComponent("Page1");

let make = _children => {
  ...component,
  initialState: () => {cells: [0, 0, 0, 1, 0, 0, 0, 0]},
  reducer: (action, _state: state) =>
    switch (action) {
    | UpdateCell(index) =>
      Js.log(string_of_int(index));
      ReasonReact.NoUpdate;
    },
  render: self => {
    let cells =
      List.map(
        cell =>
          <div className=("cell " ++ (cell === 1 ? "alive" : ""))>
            (ReasonReact.string(string_of_int(cell)))
          </div>,
        self.state.cells,
      );
    <div className="Page1">
      <div className="container simple">
        <div className="controls">
          <button className="navButton">
            (ReasonReact.string("Play"))
          </button>
          <button className="navButton">
            (ReasonReact.string("Pause"))
          </button>
        </div>
        <div className="cellContainer">
          (ReasonReact.array(Array.of_list(cells)))
        </div>
      </div>
    </div>;
  },
};