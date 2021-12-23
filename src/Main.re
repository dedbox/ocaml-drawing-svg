open Bonsai_web;
open Sexplib.Std;

module Model = {
  [@deriving sexp]
  type t = unit;
  let equal = (==);
  let default: t = ();
};

let _: Start.Handle.t(_) =
  Start.start_standalone(
    ~initial_input=(),
    ~bind_to_element_with_id="container",
    Bonsai.const(
      Drawing.(
        circle
        |> r(px(3.0))
        |> cx(px(150.0))
        |> cy(px(125.0))
        |> fill(red)
        |> to_html
      ),
    ),
  );
