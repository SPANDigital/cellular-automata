let component = ReasonReact.statelessComponent("RulesSelect");

let make = (~activeRuleset, ~sendAction, ~action, ~rulesets, _children) => {
  ...component,
  render: _self =>
    <select
      value=activeRuleset
      onChange=(
        event =>
          ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value
          |> ((rulecode: string) => sendAction(action(rulecode)))
      )>
      (
        rulesets
        |> List.map(rule => {
             let (rulecode, _) = rule;
             <option key=rulecode> (ReasonReact.string(rulecode)) </option>;
           })
        |> Array.of_list
        |> ReasonReact.array
      )
    </select>,
};