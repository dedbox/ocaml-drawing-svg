module T = Svg_types;

type t = T.paint;

let custom_color = (spec: string): t => `Color((spec, None));

let rgb = (r: Length.t, g: Length.t, b: Length.t): t =>
  Printf.sprintf(
    format_of_string("rgb(%s,%s,%s)"),
    Length.string_of_t(r),
    Length.string_of_t(g),
    Length.string_of_t(b),
  )
  |> custom_color;

let hex = (n: int): t =>
  Printf.sprintf(format_of_string("#%x"), n) |> custom_color;

let named_color = (name: string): t => `Color((name, None));
let none: t = `None;
let current_color: t = `CurrentColor;
let red: t = named_color("red");
let green: t = named_color("green");
let blue: t = named_color("blue");
let cyan: t = named_color("cyan");
let magenta: t = named_color("magenta");
let yellow: t = named_color("yellow");
let black: t = named_color("black");
let white: t = named_color("white");
let aqua: t = named_color("aqua");
let orange: t = named_color("orange");
