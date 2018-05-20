[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "../../assets/logo.svg";

let component = ReasonReact.statelessComponent("App");

let make = (~currentRoute, _children) => {
  ...component,
  render: _self =>
    <div className="App">
      <div className="App-header">
        <div className="logo-container">
          <img src=logo className="App-logo" alt="logo" />
          <svg width="769px" height="777px" viewBox="0 0 769 777">
            <g
              id="Page-1"
              stroke="none"
              strokeWidth="1"
              fill="none"
              fillRule="evenodd">
              <g id="automata-icon">
                <g id="automata" transform="translate(17.000000, 15.000000)">
                  <polygon
                    id="center"
                    stroke="#979797"
                    fill="#DB4D3F"
                    transform="translate(368.000000, 373.139144) rotate(-360.000000) translate(-368.000000, -373.139144) "
                    points="368 312 422 342.569572 422 403.708716 368 434.278288 314 403.708716 314 342.569572"
                  />
                  <polygon
                    id="poly-8"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(54.000000, 367.139144) rotate(-360.000000) translate(-54.000000, -367.139144) "
                    points="54 306 108 336.569572 108 397.708716 54 428.278288 6.57252031e-14 397.708716 3.01980663e-14 336.569572"
                  />
                  <polygon
                    id="poly-7"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(162.000000, 683.139144) rotate(-360.000000) translate(-162.000000, -683.139144) "
                    points="162 622 216 652.569572 216 713.708716 162 744.278288 108 713.708716 108 652.569572"
                  />
                  <polygon
                    id="poly-6"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(368.000000, 683.139144) rotate(-360.000000) translate(-368.000000, -683.139144) "
                    points="368 622 422 652.569572 422 713.708716 368 744.278288 314 713.708716 314 652.569572"
                  />
                  <polygon
                    id="poly-5"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(573.000000, 683.139144) rotate(-360.000000) translate(-573.000000, -683.139144) "
                    points="573 622 627 652.569572 627 713.708716 573 744.278288 519 713.708716 519 652.569572"
                  />
                  <polygon
                    id="poly-4"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(681.000000, 367.139144) rotate(-360.000000) translate(-681.000000, -367.139144) "
                    points="681 306 735 336.569572 735 397.708716 681 428.278288 627 397.708716 627 336.569572"
                  />
                  <polygon
                    id="poly-3"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(573.000000, 61.139144) rotate(-360.000000) translate(-573.000000, -61.139144) "
                    points="573 1.42108547e-14 627 30.569572 627 91.7087161 573 122.278288 519 91.7087161 519 30.569572"
                  />
                  <polygon
                    id="poly-2"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(368.000000, 61.139144) rotate(-360.000000) translate(-368.000000, -61.139144) "
                    points="368 1.42108547e-14 422 30.569572 422 91.7087161 368 122.278288 314 91.7087161 314 30.569572"
                  />
                  <polygon
                    id="poly-1"
                    stroke="#DB4D3F"
                    strokeWidth="2"
                    fill="#222222"
                    transform="translate(162.000000, 61.139144) rotate(-360.000000) translate(-162.000000, -61.139144) "
                    points="162 -8.8817842e-16 216 30.569572 216 91.7087161 162 122.278288 108 91.7087161 108 30.569572"
                  />
                </g>
              </g>
            </g>
          </svg>
        </div>
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