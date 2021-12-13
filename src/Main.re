module App = {
  open Incr_dom;
  open Sexplib.Std;

  module Model = {
    type t = unit;
    let cutoff: (t, t) => bool = (===);
    let init: t = ();
  };

  module Action = {
    [@deriving sexp]
    type t = unit;
  };

  module State = {
    type t = unit;
    let init = ();
  };

  module View = {
    open Virtual_dom.Vdom;

    module Content = {
      open Virtual_dom.Tyxml.Svg;

      let msg =
        text(
          ~a=[
            a_x_list([(150.0, None)]),
            a_y_list([(125.0, None)]),
            a_font_size("30"),
            a_fill(`Color(("white", None))),
          ],
          [txt("hey ho")],
        );
    };

    let init: Node.t = {
      Virtual_dom.Tyxml.(
        Html.(svg(~a=Svg.[a_id("container")], [Content.msg]) |> toelt)
      );
    };
  };

  let on_startup =
      (~schedule_action as _: Action.t => unit, _: Model.t)
      : Async_kernel.Deferred.t(State.t) =>
    Async_kernel.Deferred.return(State.init);

  let create =
      (
        model: Incr.t(Model.t),
        ~old_model as _: Incr.t(Model.t),
        ~inject as _: Action.t => Vdom.Event.t,
      )
      : Incr.t(Component.t(Action.t, Model.t, State.t)) => {
    open Incr.Let_syntax;
    let%map model = model;
    let apply_action = (_action, _state, ~schedule_action as _) => model;
    let view = View.init;
    Component.create(~apply_action, model, view);
  };
};

Incr_dom.Start_app.start(
  (module App),
  ~bind_to_element_with_id="container",
  ~initial_model=App.Model.init,
);
