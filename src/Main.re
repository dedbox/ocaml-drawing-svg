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
    // open Gg;

    // let init =
    //   empty
    //   |> id("container")
    //   |> push(
    //        text("Hey there!!")
    //        |> x(150)
    //        |> y(125)
    //        |> font_size(30)
    //        |> fill("white"),
    //      );

    module Attr = Virtual_dom.Vdom.Attr;
    module Node = Virtual_dom.Vdom.Node;
    module SAttr = Virtual_dom_svg.Attr;
    module SNode = Virtual_dom_svg.Node;

    let font_size = (size: int): Attr.t =>
      Attr.create("font-size", Int.to_string(size));
    let init = {
      SNode.svg(
        [Attr.id("container")],
        [
          SNode.text(
            SAttr.[
              x(150.0),
              y(125.0),
              font_size(30),
              fill(`Name("white")),
            ],
            [Node.text("Hello again!!")],
          ),
        ],
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
