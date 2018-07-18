[%bs.raw {|require('./home.scss')|}];

[@bs.module] external ca_board : string = "../../assets/ca-board.png";

[@bs.module] external simple_grid : string = "../../assets/simple-grid.png";

[@bs.module]
external simple_states : string = "../../assets/simple-states.png";

[@bs.module]
external simple_neighborhood : string = "../../assets/simple-neighborhood.png";

[@bs.module]
external simple_gen_calc : string = "../../assets/simple-gen-calc.png";

[@bs.module] external binary : string = "../../assets/binary.png";

[@bs.module]
external binary_to_ruleset : string = "../../assets/binary-to-ruleset.png";

[@bs.module] external simple_init : string = "../../assets/simple-init.png";

[@bs.module]
external gen_calc_ruleset : string = "../../assets/gen-calc-ruleset.png";

[@bs.module]
external vis_ruleset : string = "../../assets/visualise-ruleset.png";

[@bs.module] external gol_grid : string = "../../assets/gol-grid.png";

[@bs.module] external gol_gen : string = "../../assets/gol-gen.png";

[@bs.module] external gol_static : string = "../../assets/gol-static.png";

[@bs.module] external gol_alt : string = "../../assets/gol-alt.png";

[@bs.module] external gol_move : string = "../../assets/gol-move.png";

let component = ReasonReact.statelessComponent("Home");

let make = _children => {
  ...component,
  render: _self =>
    <div className="Home">
      <div className="container">
        <section>
          <h1> ("1. What is Cellular Automata?" |> ReasonReact.string) </h1>
          <h3>
            (
              "A cellular automaton is a model of a system of 'cell' objects with the following characteristics."
              |> ReasonReact.string
            )
          </h3>
          <ul>
            <li>
              (
                "The cells live on a grid. (We'll see examples in both one and two dimensions in this chapter, though a cellular automaton can exist in any finite number of dimensions.)"
                |> ReasonReact.string
              )
            </li>
            <li>
              (
                "Each cell has a state. The number of state possibilities is typically finite. The simplest example has the two possibilities of 1 and 0 (otherwise referred to as 'on' and 'off' or 'alive' and 'dead')."
                |> ReasonReact.string
              )
            </li>
            <li>
              (
                "Each cell has a neighborhood. This can be defined in any number of ways, but it is typically a list of adjacent cells."
                |> ReasonReact.string
              )
            </li>
          </ul>
          <img src=ca_board />
        </section>
        <section>
          <h1> ("2. Elementary Cellular Automata" |> ReasonReact.string) </h1>
          <ol>
            <li>
              <strong> (ReasonReact.string("Grid.")) </strong>
              (
                "The simplest grid would be one-dimensional: a line of cells."
                |> ReasonReact.string
              )
              <img src=simple_grid />
            </li>
            <li>
              <strong> (ReasonReact.string("States.")) </strong>
              (
                "The simplest set of states (beyond having only one state) would be two states: 0 or 1."
                |> ReasonReact.string
              )
              <img src=simple_states />
            </li>
            <li>
              <strong> (ReasonReact.string("Neighborhood.")) </strong>
              (
                "The simplest neighborhood in one dimension for any given cell would be the cell itself and its two adjacent neighbors: one to the left and one to the right."
                |> ReasonReact.string
              )
              <img src=simple_neighborhood />
            </li>
          </ol>
          <h3>
            (
              "Let's say we have an individual cell in the CA, and let's call it CELL. The formula for calculating CELL's state at any given time t is as follows:"
              |> ReasonReact.string
            )
          </h3>
          <pre>
            (
              ReasonReact.string(
                "CELL state at time t = f(CELL neighborhood at time t - 1)",
              )
            )
          </pre>
          <p>
            (
              "In other words, a cell's new state is a function of all the states in the cell's neighborhood at the previous moment in time (or during the previous generation). We calculate a new state value by looking at all the previous neighbor states."
              |> ReasonReact.string
            )
          </p>
          <img src=simple_gen_calc />
          <p>
            (
              "We have three cells, each with a state of 0 or 1. How many possible ways can we configure the states? If you love binary, you'll notice that three cells define a 3-bit number, and how high can you count with 3 bits? Up to 8."
              |> ReasonReact.string
            )
          </p>
          <img src=binary />
          <p>
            (
              "Once we have defined all the possible neighborhoods, we need to define an outcome (new state value: 0 or 1) for each neighborhood configuration."
              |> ReasonReact.string
            )
          </p>
          <img src=binary_to_ruleset />
          <p>
            (
              "The standard Wolfram model is to start generation 0 with all cells having a state of 0 except for the middle cell, which should have a state of 1."
              |> ReasonReact.string
            )
          </p>
          <img src=simple_init />
          <p>
            (
              "Referring to the ruleset above, let\226\128\153s see how a given cell (we\226\128\153ll pick the center one) would change from generation 0 to generation 1."
              |> ReasonReact.string
            )
          </p>
          <img src=gen_calc_ruleset />
          <h3>
            (
              "So in conclusion you can visualise these rulesets as follows:"
              |> ReasonReact.string
            )
          </h3>
          <img src=vis_ruleset />
        </section>
        <section>
          <h1> ("3. The Game of Life" |> ReasonReact.string) </h1>
          <p>
            (
              {|
             First, instead of a line of cells, we now have a two-dimensional matrix of cells. As with the elementary CA, the possible states are 0 or 1. Only in this case, since we’re talking about 'life,' 0 means dead and 1 means alive

             The cell’s neighborhood has also expanded. If a neighbor is an adjacent cell, a neighborhood is now nine cells instead of three.

             With three cells, we had a 3-bit number or eight possible configurations. With nine cells, we have 9 bits, or 512 possible neighborhoods. In most cases, it would be impractical to define an outcome for every single possibility.
             The Game of Life gets around this problem by defining a set of rules according to general characteristics of the neighborhood. In other words, is the neighborhood overpopulated with life? Surrounded by death? Or just right? Here are the rules of life.
            |}
              |> ReasonReact.string
            )
          </p>
          <img src=gol_grid className="gol-grid" />
          <ol>
            <li>
              <strong> (ReasonReact.string("Death.")) </strong>
              (
                "If a cell is alive (state = 1) it will die (state becomes 0) under the following circumstances."
                |> ReasonReact.string
              )
              <ul>
                <li>
                  <strong> (ReasonReact.string("Overpopulation:")) </strong>
                  (
                    "If the cell has four or more alive neighbors, it dies."
                    |> ReasonReact.string
                  )
                </li>
                <li>
                  <strong> (ReasonReact.string("Loneliness:")) </strong>
                  (
                    "If the cell has one or fewer alive neighbors, it dies."
                    |> ReasonReact.string
                  )
                </li>
              </ul>
            </li>
          </ol>
          <ol>
            <li>
              <strong> (ReasonReact.string("Birth.")) </strong>
              (
                "If a cell is dead (state = 0) it will come to life (state becomes 1) if it has exactly three alive neighbors (no more, no less)."
                |> ReasonReact.string
              )
            </li>
          </ol>
          <ol>
            <li>
              <strong> (ReasonReact.string("Stasis.")) </strong>
              (
                "In all other cases, the cell state does not change. To be thorough, let\226\128\153s describe those scenarios."
                |> ReasonReact.string
              )
              <ul>
                <li>
                  <strong> (ReasonReact.string("Staying Alive:")) </strong>
                  (
                    "If a cell is alive and has exactly two or three live neighbors, it stays alive."
                    |> ReasonReact.string
                  )
                </li>
                <li>
                  <strong> (ReasonReact.string("Staying Dead:")) </strong>
                  (
                    "If a cell is dead and has anything other than three live neighbors, it stays dead."
                    |> ReasonReact.string
                  )
                </li>
              </ul>
            </li>
          </ol>
          <img src=gol_gen />
          <p>
            (
              "One of the exciting aspects of the Game of Life is that there are initial patterns that yield intriguing results. For example, some remain static and never change."
              |> ReasonReact.string
            )
          </p>
          <img src=gol_static />
          <p>
            (
              "There are patterns that oscillate back and forth between two states."
              |> ReasonReact.string
            )
          </p>
          <img src=gol_alt />
          <p>
            (
              "And there are also patterns that from generation to generation move about the grid. (It's important to note that the cells themselves aren't actually moving, although we see the appearance of motion in the result as the cells turn on and off.)"
              |> ReasonReact.string
            )
          </p>
          <img src=gol_move />
          <h3>
            (
              ReasonReact.string(
                "The End: These examples where taken from the exellent book by Daniel Shiffman | ",
              )
            )
            <a href="https://natureofcode.com/" target="_blank">
              (ReasonReact.string("The Nature of Code"))
            </a>
          </h3>
        </section>
      </div>
    </div>,
};