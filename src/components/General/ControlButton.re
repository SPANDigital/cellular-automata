let component = ReasonReact.statelessComponent("ControlButton");

let make = (~active: bool, ~sendAction, ~action, ~label: string, _children) => {
  ...component,
  render: _self =>
    <button
      className=("navButton " ++ (active ? "active" : ""))
      onClick=(_event => sendAction(action))>
      (ReasonReact.string(label))
    </button>,
};