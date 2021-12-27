module NE = NonEmptyList;
module C = Coordinate;

type positioning =
  | Absolute
  | Relative;

type cubic_args = (C.pair, C.pair, C.pair);
type smooth_cubic_args = (C.pair, C.pair);
type quadratic_args = (C.pair, C.pair);
type arc_args = (C.pair, C.t, bool, bool, C.pair);

type action =
  | Move(NE.t(C.pair))
  | Line(NE.t(C.pair))
  | Horizontal(NE.t(C.t))
  | Vertical(NE.t(C.t))
  | Cubic(NE.t(cubic_args))
  | SmoothCubic(NE.t(smooth_cubic_args))
  | Quadratic(NE.t(quadratic_args))
  | SmoothQuadratic(NE.t(C.pair))
  | Arc(NE.t(arc_args));

type command = (positioning, action);

type final = {
  closed: bool,
  absolute: bool,
};

type t = {
  initial: NE.t(C.pair),
  inner: list(command),
  closed: bool,
};

let string_of_absolute_action: action => string =
  fun
  | Move(_) => "M"
  | Line(_) => "L"
  | Horizontal(_) => "H"
  | Vertical(_) => "V"
  | Cubic(_) => "C"
  | SmoothCubic(_) => "S"
  | Quadratic(_) => "Q"
  | SmoothQuadratic(_) => "T"
  | Arc(_) => "A";

let string_of_relative_action: action => string =
  fun
  | Move(_) => "m"
  | Line(_) => "l"
  | Horizontal(_) => "h"
  | Vertical(_) => "v"
  | Cubic(_) => "c"
  | SmoothCubic(_) => "s"
  | Quadratic(_) => "q"
  | SmoothQuadratic(_) => "t"
  | Arc(_) => "a";

let string_of_action = (positioning: positioning, action: action): string =>
  switch (positioning) {
  | Absolute => action |> string_of_absolute_action
  | Relative => action |> string_of_relative_action
  };

let string_of_action_args: action => string =
  fun
  | Move(ps) => ps |> C.string_of_nonempty_pairs
  | Line(ps) => ps |> C.string_of_nonempty_pairs
  | Horizontal(cs) => cs |> C.string_of_nonempty_list
  | Vertical(cs) => cs |> C.string_of_nonempty_list
  | Cubic(args) => args |> NE.string_of_t(C.string_of_pairs3)
  | SmoothCubic(args) => args |> NE.string_of_t(C.string_of_pairs2)
  | Quadratic(args) => args |> NE.string_of_t(C.string_of_pairs2)
  | SmoothQuadratic(ps) => ps |> C.string_of_nonempty_pairs
  | Arc(args) =>
    args
    |> NE.string_of_t((((c1, c2), c3, f1, f2, p)) => {
         let cstr = [c1, c2, c3] |> C.string_of_list;
         let fstr =
           [f1, f2] |> List.map(f => f ? "1" : "0") |> String.concat(" ");
         let pstr = C.string_of_pair(p);
         [cstr, fstr, pstr] |> String.concat(" ");
       });

let string_of_command = ((positioning, action): command): string =>
  string_of_action(positioning, action)
  ++ " "
  ++ string_of_action_args(action);

let string_of_t = ({initial, inner, closed}: t): string => {
  let ci = (Absolute, Move(initial)) |> string_of_command;
  let cs = inner |> List.map(string_of_command);
  let cf = closed ? ["Z"] : [];
  [ci, ...cs] @ cf |> String.concat(" ");
};

let init = (~tail: list(C.pair)=[], p: C.pair): t => {
  let initial = NE.init(p, ~tail);
  {initial, inner: [], closed: false};
};

let of_pairs = (ps: list(C.pair)): t =>
  switch (ps) {
  | [p, ...tail] => init(p, ~tail)
  | [] => failwith(__LOC__ ++ ": not a non-empty list")
  };

let of_int_pairs = (ps: list((int, int))): t =>
  switch (ps) {
  | [p, ...tail] => init(C.int_pair(p), ~tail=C.int_pairs(tail))
  | [] => failwith(__LOC__ ++ ": not a non-empty list")
  };

let of_float_pairs = (ps: list((float, float))): t =>
  switch (ps) {
  | [p, ...tail] => init(C.float_pair(p), ~tail=C.float_pairs(tail))
  | [] => failwith(__LOC__ ++ ": not a non-empty list")
  };

let draw = (path: t): Drawing.t('a, 'b, 'c) =>
  Shape.path |> Attribute.d(path |> string_of_t);

let close = ({initial, inner, _}: t): t => {initial, inner, closed: true};

let move =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(C.pair)=[],
      p: C.pair,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(p, ~tail);
  switch (inner) {
  | [] =>
    let initial = NE.append(initial, new_args);
    {initial, inner, closed};
  | [(pos, Move(ps)), ...cmds] when pos == positioning =>
    let ps' = NE.append(ps, new_args);
    let inner = [(pos, Move(ps')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Move((p, []))), ...inner];
    {initial, inner, closed};
  };
};

let line =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(C.pair)=[],
      p: C.pair,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(p, ~tail);
  switch (inner) {
  | [] =>
    let initial = NE.(append(initial, new_args));
    {initial, inner, closed};
  | [(pos, Move(ps)), ...cmds] when pos == positioning =>
    let ps' = NE.append(ps, new_args);
    let inner = [(pos, Move(ps')), ...cmds];
    {initial, inner, closed};
  | [(pos, Line((p', ps'))), ...cmds] when pos == positioning =>
    let inner = [(pos, Line((p', ps' @ [p]))), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Line((p, []))), ...inner];
    {initial, inner, closed};
  };
};

let horizontal =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(C.t)=[],
      c: C.t,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(c, ~tail);
  switch (inner) {
  | [(pos, Horizontal(cs)), ...cmds] when pos == positioning =>
    let cs' = NE.append(cs, new_args);
    let inner = [(pos, Horizontal(cs')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Horizontal(new_args))];
    {initial, inner, closed};
  };
};

let vertical =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(C.t)=[],
      c: C.t,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(c, ~tail);
  switch (inner) {
  | [(pos, Vertical(cs)), ...cmds] when pos == positioning =>
    let cs' = NE.append(cs, new_args);
    let inner = [(pos, Vertical(cs')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Vertical(new_args))];
    {initial, inner, closed};
  };
};

let cubic =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(cubic_args)=[],
      args: cubic_args,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(args, ~tail);
  switch (inner) {
  | [(pos, Cubic(args)), ...cmds] when pos == positioning =>
    let args' = NE.(append(args, new_args));
    let inner = [(pos, Cubic(args')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Cubic(new_args))];
    {initial, inner, closed};
  };
};

let smooth_cubic =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(smooth_cubic_args)=[],
      args: smooth_cubic_args,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(args, ~tail);
  switch (inner) {
  | [(pos, SmoothCubic(args)), ...cmds] when pos == positioning =>
    let args' = NE.append(args, new_args);
    let inner = [(pos, SmoothCubic(args')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, SmoothCubic(new_args))];
    {initial, inner, closed};
  };
};

let quadratic =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(quadratic_args)=[],
      args: quadratic_args,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(args, ~tail);
  switch (inner) {
  | [(pos, Quadratic(args)), ...cmds] when pos == positioning =>
    let args' = NE.append(args, new_args);
    let inner = [(pos, Quadratic(args')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Quadratic(new_args))];
    {initial, inner, closed};
  };
};

let smooth_quadratic =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(C.pair)=[],
      p: C.pair,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(p, ~tail);
  switch (inner) {
  | [(pos, SmoothQuadratic(args)), ...cmds] when pos == positioning =>
    let args' = NE.append(args, new_args);
    let inner = [(pos, SmoothQuadratic(args')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, SmoothQuadratic(new_args))];
    {initial, inner, closed};
  };
};

let arc =
    (
      ~positioning: positioning=Absolute,
      ~tail: list(arc_args)=[],
      args: arc_args,
      {initial, inner, closed}: t,
    )
    : t => {
  let new_args = NE.init(args, ~tail);
  switch (inner) {
  | [(pos, Arc(args)), ...cmds] when pos == positioning =>
    let args' = NE.append(args, new_args);
    let inner = [(pos, Arc(args')), ...cmds];
    {initial, inner, closed};
  | _ =>
    let inner = [(positioning, Arc(new_args))];
    {initial, inner, closed};
  };
};

let move_rel: (C.pair, t) => t = move(~positioning=Relative);
let line_rel: (C.pair, t) => t = line(~positioning=Relative);
let horizontal_rel: (C.t, t) => t = horizontal(~positioning=Relative);
let vertical_rel: (C.t, t) => t = vertical(~positioning=Relative);
let cubic_rel: (cubic_args, t) => t = cubic(~positioning=Relative);
let smooth_cubic_rel: (smooth_cubic_args, t) => t =
  smooth_cubic(~positioning=Relative);
let quadratic_rel: (quadratic_args, t) => t =
  quadratic(~positioning=Relative);
let smooth_quadratic_rel: (C.pair, t) => t =
  smooth_quadratic(~positioning=Relative);
let arc_rel: (arc_args, t) => t = arc(~positioning=Relative);

let moves = (ps: NE.t(C.pair)): (t => t) => {
  let (p, tail) = NE.peek(ps);
  move(~tail, p);
};
let lines = (ps: NE.t(C.pair)): (t => t) => {
  let (p, tail) = NE.peek(ps);
  line(~tail, p);
};
let horizontals = (cs: NE.t(C.t)): (t => t) => {
  let (c, tail) = NE.peek(cs);
  horizontal(~tail, c);
};
let verticals = (cs: NE.t(C.t)): (t => t) => {
  let (c, tail) = NE.peek(cs);
  vertical(~tail, c);
};
let cubics = (args: NE.t(cubic_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  cubic(~tail, arg);
};
let smoth_cubics = (args: NE.t(smooth_cubic_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  smooth_cubic(~tail, arg);
};
let quadratics = (args: NE.t(quadratic_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  quadratic(~tail, arg);
};
let smooth_quadratics = (ps: NE.t(C.pair)): (t => t) => {
  let (p, tail) = NE.peek(ps);
  smooth_quadratic(~tail, p);
};
let arcs = (args: NE.t(arc_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  arc(~tail, arg);
};

let moves_rel = (ps: NE.t(C.pair)): (t => t) => {
  let (p, tail) = NE.peek(ps);
  move(~positioning=Relative, ~tail, p);
};
let lines_rel = (ps: NE.t(C.pair)): (t => t) => {
  let (p, tail) = NE.peek(ps);
  line(~positioning=Relative, ~tail, p);
};
let horizontals_rel = (cs: NE.t(C.t)): (t => t) => {
  let (c, tail) = NE.peek(cs);
  horizontal(~positioning=Relative, ~tail, c);
};
let verticals_rel = (cs: NE.t(C.t)): (t => t) => {
  let (c, tail) = NE.peek(cs);
  vertical(~positioning=Relative, ~tail, c);
};
let cubics_rel = (args: NE.t(cubic_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  cubic(~positioning=Relative, ~tail, arg);
};
let smoth_cubics_rel = (args: NE.t(smooth_cubic_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  smooth_cubic(~positioning=Relative, ~tail, arg);
};
let quadratics_rel = (args: NE.t(quadratic_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  quadratic(~positioning=Relative, ~tail, arg);
};
let smooth_quadratics_rel = (ps: NE.t(C.pair)): (t => t) => {
  let (p, tail) = NE.peek(ps);
  smooth_quadratic(~positioning=Relative, ~tail, p);
};
let arcs_rel = (args: NE.t(arc_args)): (t => t) => {
  let (arg, tail) = NE.peek(args);
  arc(~positioning=Relative, ~tail, arg);
};
