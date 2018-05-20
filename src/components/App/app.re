[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "../../assets/logo.svg";

let component = ReasonReact.statelessComponent("App");

let make = (~currentRoute, _children) => {
  ...component,
  render: _self =>
    <div className="App">
      <div className="App-header">
        <img src=logo className="App-logo" alt="logo" />
        <h2>
          (
            ReasonReact.string("Cellular Automata: Simulated with ReasonReact")
          )
        </h2>
      </div>
      <nav>
        <Router.NavLink route=Home className="navButton">
          (ReasonReact.string("What is Cellular Automata?"))
        </Router.NavLink>
        <Router.NavLink route=Page1 className="navButton">
          (ReasonReact.string("Simulation"))
        </Router.NavLink>
      </nav>
      <main>
        <ReactTransitionGroup.TransitionGroup>
          (Config.routeToComponent(currentRoute))
        </ReactTransitionGroup.TransitionGroup>
      </main>
    </div>,
};