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
      Virtual_dom.Tyxml.(
        Html.svg([
          Svg.text(
            ~a=[
              Svg.a_x_list([(150.0, None)]),
              Svg.a_y_list([(125.0, None)]),
              Svg.a_font_size("30"),
              Svg.a_fill(`Color(("white", None))),
            ],
            [Svg.txt("hey ho hey!!")],
          ),
        ])
        |> Html.toelt
      ),
    ),
  );
