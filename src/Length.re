module T = Svg_types;

type t = T.Unit.length;

let float = (x: float): t => (x, None);
let int = (x: int): t => (x |> Float.of_int, None);

let floats = (xs: list(float)): list(t) => xs |> List.map(float);
let ints = (xs: list(int)): list(t) => xs |> List.map(int);

let em = (x: float): t => (x, Some(`Em));
let ex = (x: float): t => (x, Some(`Ex));
let px = (x: float): t => (x, Some(`Px));
let inches = (x: float): t => (x, Some(`In));
let cm = (x: float): t => (x, Some(`Cm));
let mm = (x: float): t => (x, Some(`Mm));
let pt = (x: float): t => (x, Some(`Pt));
let pc = (x: float): t => (x, Some(`Pc));
let percent = (x: int): t => (x |> Int.to_float, Some(`Percent));
let percentf = (x: float): t => (x, Some(`Percent));

let string_of_t = ((x, unit_opt): t): string =>
  switch (unit_opt) {
  | None => x |> Float.to_int |> Int.to_string
  | Some(unit) =>
    Printf.sprintf(
      format_of_string("%f%s"),
      x,
      switch (unit) {
      | `Em => "em"
      | `Ex => "ex"
      | `Px => "px"
      | `In => "in"
      | `Cm => "cm"
      | `Mm => "mm"
      | `Pt => "pt"
      | `Pc => "pc"
      | `Percent => "%"
      },
    )
  };
