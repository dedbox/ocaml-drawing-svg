module NE = NonEmptyList;

module type CoordinateType = {
  type t;
  let string_of_t: t => string;
};

module Make = (C: CoordinateType) => {
  open Positioning;

  type coordinate = C.t;

  type pair = (coordinate, coordinate);

  type cubic = (pair, pair, pair);
  type smooth_cubic = (pair, pair);
  type quadratic = (pair, pair);
  type arc = (pair, coordinate, bool, bool, pair);

  type action =
    | Move(NE.t(pair))
    | Line(NE.t(pair))
    | Horizontal(NE.t(coordinate))
    | Vertical(NE.t(coordinate))
    | Cubic(NE.t(cubic))
    | SmoothCubic(NE.t(smooth_cubic))
    | Quadratic(NE.t(quadratic))
    | SmoothQuadratic(NE.t(pair))
    | Arc(NE.t(arc));

  type command = (Positioning.t, action);

  type final = {
    closed: bool,
    absolute: bool,
  };

  type t = {
    initial: NE.t(pair),
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

  let string_of_action = (positioning: Positioning.t, action: action): string =>
    switch (positioning) {
    | Absolute => action |> string_of_absolute_action
    | Relative => action |> string_of_relative_action
    };

  let string_of_pair = ((c1, c2): pair): string =>
    NE.init(c1, ~tail=[c2]) |> NE.string_of_t(C.string_of_t);

  let string_of_pairs = (ps: list(pair)): string =>
    ps |> List.map(string_of_pair) |> String.concat(" ");

  let string_of_pairs2 = ((p1: pair, p2: pair)): string =>
    [p1, p2] |> string_of_pairs;

  let string_of_pairs3 = ((p1: pair, p2: pair, p3: pair)): string =>
    [p1, p2, p3] |> string_of_pairs;

  let string_of_nonempty_pairs = (ps: NE.t(pair)): string =>
    ps |> NE.string_of_t(string_of_pair);

  let string_of_coordinates = (cs: list(coordinate)): string =>
    cs |> List.map(C.string_of_t) |> String.concat(" ");

  let string_of_nonempty_coordinates = (cs: NE.t(coordinate)): string =>
    cs |> NE.string_of_t(C.string_of_t);

  let string_of_action_args: action => string =
    fun
    | Move(ps) => string_of_nonempty_pairs(ps)
    | Line(ps) => string_of_nonempty_pairs(ps)
    | Horizontal(cs) => string_of_nonempty_coordinates(cs)
    | Vertical(cs) => string_of_nonempty_coordinates(cs)
    | Cubic(args) => args |> NE.string_of_t(string_of_pairs3)
    | SmoothCubic(args) => args |> NE.string_of_t(string_of_pairs2)
    | Quadratic(args) => args |> NE.string_of_t(string_of_pairs2)
    | SmoothQuadratic(ps) => string_of_nonempty_pairs(ps)
    | Arc(args) =>
      args
      |> NE.string_of_t((((c1, c2), c3, f1, f2, p)) => {
           let cstr = [c1, c2, c3] |> string_of_coordinates;
           let fstr =
             [f1, f2] |> List.map(f => f ? "1" : "0") |> String.concat(" ");
           let pstr = string_of_pair(p);
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

  let init = (~tail: list(pair)=[], p: pair): t => {
    let initial = NE.init(p, ~tail);
    {initial, inner: [], closed: false};
  };

  let of_pairs = (ps: list(pair)): t =>
    switch (ps) {
    | [p, ...tail] => init(p, ~tail)
    | [] => failwith(__LOC__ ++ ": must have at least one pair")
    };

  let draw = (path: t): Drawing.t('a, 'b, 'c) =>
    Shape.path |> Attribute.d(path |> string_of_t);

  let close = ({initial, inner, _}: t): t => {initial, inner, closed: true};
  let closed = close;

  let move =
      (
        ~positioning: Positioning.t=Absolute,
        ~tail: list(pair)=[],
        p: pair,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(pair)=[],
        p: pair,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(coordinate)=[],
        c: coordinate,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(coordinate)=[],
        c: coordinate,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(cubic)=[],
        args: cubic,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(smooth_cubic)=[],
        args: smooth_cubic,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(quadratic)=[],
        args: quadratic,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(pair)=[],
        p: pair,
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
        ~positioning: Positioning.t=Absolute,
        ~tail: list(arc)=[],
        args: arc,
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

  let move_rel: (pair, t) => t = move(~positioning=Relative);
  let line_rel: (pair, t) => t = line(~positioning=Relative);
  let horizontal_rel: (coordinate, t) => t =
    horizontal(~positioning=Relative);
  let vertical_rel: (coordinate, t) => t = vertical(~positioning=Relative);
  let cubic_rel: (cubic, t) => t = cubic(~positioning=Relative);
  let smooth_cubic_rel: (smooth_cubic, t) => t =
    smooth_cubic(~positioning=Relative);
  let quadratic_rel: (quadratic, t) => t = quadratic(~positioning=Relative);
  let smooth_quadratic_rel: (pair, t) => t =
    smooth_quadratic(~positioning=Relative);
  let arc_rel: (arc, t) => t = arc(~positioning=Relative);

  let moves = (ps: NE.t(pair)): (t => t) => {
    let (p, tail) = NE.peek(ps);
    move(~tail, p);
  };
  let lines = (ps: NE.t(pair)): (t => t) => {
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
  let cubics = (args: NE.t(cubic)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    cubic(~tail, arg);
  };
  let smoth_cubics = (args: NE.t(smooth_cubic)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    smooth_cubic(~tail, arg);
  };
  let quadratics = (args: NE.t(quadratic)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    quadratic(~tail, arg);
  };
  let smooth_quadratics = (ps: NE.t(pair)): (t => t) => {
    let (p, tail) = NE.peek(ps);
    smooth_quadratic(~tail, p);
  };
  let arcs = (args: NE.t(arc)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    arc(~tail, arg);
  };

  let moves_rel = (ps: NE.t(pair)): (t => t) => {
    let (p, tail) = NE.peek(ps);
    move(~positioning=Relative, ~tail, p);
  };
  let lines_rel = (ps: NE.t(pair)): (t => t) => {
    let (p, tail) = NE.peek(ps);
    line(~positioning=Relative, ~tail, p);
  };
  let horizontals_rel = (cs: NE.t(coordinate)): (t => t) => {
    let (c, tail) = NE.peek(cs);
    horizontal(~positioning=Relative, ~tail, c);
  };
  let verticals_rel = (cs: NE.t(coordinate)): (t => t) => {
    let (c, tail) = NE.peek(cs);
    vertical(~positioning=Relative, ~tail, c);
  };
  let cubics_rel = (args: NE.t(cubic)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    cubic(~positioning=Relative, ~tail, arg);
  };
  let smoth_cubics_rel = (args: NE.t(smooth_cubic)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    smooth_cubic(~positioning=Relative, ~tail, arg);
  };
  let quadratics_rel = (args: NE.t(quadratic)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    quadratic(~positioning=Relative, ~tail, arg);
  };
  let smooth_quadratics_rel = (ps: NE.t(pair)): (t => t) => {
    let (p, tail) = NE.peek(ps);
    smooth_quadratic(~positioning=Relative, ~tail, p);
  };
  let arcs_rel = (args: NE.t(arc)): (t => t) => {
    let (arg, tail) = NE.peek(args);
    arc(~positioning=Relative, ~tail, arg);
  };
};
