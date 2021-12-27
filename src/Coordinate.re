module NE = NonEmptyList;

type t =
  | Int(int)
  | Float(float);

type pair = (t, t);

let string_of_t: t => string =
  fun
  | Int(n) => Int.to_string(n)
  | Float(n) => Float.to_string(n);

let string_of_list = (cs: list(t)): string =>
  cs |> List.map(string_of_t) |> String.concat(" ");

let string_of_nonempty_list = (cs: NE.t(t)): string =>
  cs |> NE.string_of_t(string_of_t);

let string_of_pair = ((c1, c2): pair): string =>
  NE.init(c1, ~tail=[c2]) |> NE.string_of_t(string_of_t);

let string_of_pairs = (ps: list(pair)): string =>
  ps |> List.map(string_of_pair) |> String.concat(" ");

let string_of_pairs2 = ((p1: pair, p2: pair)): string =>
  [p1, p2] |> string_of_pairs;

let string_of_pairs3 = ((p1: pair, p2: pair, p3: pair)): string =>
  [p1, p2, p3] |> string_of_pairs;

let string_of_pairs4 = ((p1: pair, p2: pair, p3: pair, p4: pair)): string =>
  [p1, p2, p3, p4] |> string_of_pairs;

let string_of_nonempty_pairs = (ps: NE.t(pair)): string =>
  ps |> NE.string_of_t(string_of_pair);

let int = (i: int): t => Int(i);
let float = (f: float) => Float(f);

let int_pair = ((i1: int, i2: int)): pair => (Int(i1), Int(i2));
let float_pair = ((f1: float, f2: float)): pair => (Float(f1), Float(f2));

let int_pairs = (ips: list((int, int))): list(pair) =>
  ips |> List.map(int_pair);
let float_pairs = (fps: list((float, float))): list(pair) =>
  fps |> List.map(float_pair);
