[%bs.raw {|require('./index.scss')|}];

[@bs.module "./RegisterServiceWorker"]
external register_service_worker : unit => unit = "default";

ReactDOMRe.renderToElementWithId(
  <Router.WithRouter>
    ...((~currentRoute) => <App currentRoute />)
  </Router.WithRouter>,
  "root",
);

register_service_worker();