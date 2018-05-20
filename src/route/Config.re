type routes =
  | Home
  | Simulation;

let routeToString =
  fun
  | Home => "/"
  | Simulation => "/simulation";

let urlToRoute: ReasonReact.Router.url => routes =
  url =>
    switch (url.path) {
    | ["simulation"] => Simulation
    | _ => Home
    };

let routeToTitle = route =>
  switch (route) {
  | Home => "Home"
  | Simulation => "Simulation"
  };

let routeToComponent = currentRoute => {
  let withCSSTransition = (component, route) =>
    <ReactTransitionGroup.CSSTransition
      show=true
      timeout=900
      key=(routeToTitle(route))
      classNames="routeTransition">
      component
    </ReactTransitionGroup.CSSTransition>;
  switch (currentRoute) {
  | Home => withCSSTransition(<Home />, Home)
  | Simulation => withCSSTransition(<Simulation />, Simulation)
  };
};