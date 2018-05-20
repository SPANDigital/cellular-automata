[%bs.raw {|require('./home.css')|}];

let component = ReasonReact.statelessComponent("Home");

let html = {|
  <h1>Home</h1>
|};

let make = _children => {
  ...component,
  render: _self =>
    <div className="Home" dangerouslySetInnerHTML={"__html": html} />,
};