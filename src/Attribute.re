open Virtual_dom.Tyxml.Svg;

type t('x, 'a, 'b, 'c) =
  ('x, Drawing.t('a, 'b, 'c)) => Drawing.t('a, 'b, 'c);

let attribute =
    (f: 'x => attrib('a), x: 'x, (a, shape, elts): Drawing.t('a, 'b, 'c))
    : Drawing.t('a, 'b, 'c) => (
  [f(x), ...a],
  shape,
  elts,
);

let r: t(Length.t, [> | `R], 'b, 'c) =
  (len, elt) => attribute(a_r, len, elt);
let cx: t(Length.t, [> | `Cx], 'b, 'c) =
  (len, elt) => attribute(a_cx, len, elt);
let cy: t(Length.t, [> | `Cy], 'b, 'c) =
  (len, elt) => attribute(a_cy, len, elt);
let rx: t(Length.t, [> | `Rx], 'b, 'c) =
  (len, elt) => attribute(a_rx, len, elt);
let ry: t(Length.t, [> | `Ry], 'b, 'c) =
  (len, elt) => attribute(a_ry, len, elt);
let x1: t(Length.t, [> | `X1], 'b, 'c) =
  (len, elt) => attribute(a_x1, len, elt);
let y1: t(Length.t, [> | `Y1], 'b, 'c) =
  (len, elt) => attribute(a_y1, len, elt);
let x2: t(Length.t, [> | `X2], 'b, 'c) =
  (len, elt) => attribute(a_x2, len, elt);
let y2: t(Length.t, [> | `Y2], 'b, 'c) =
  (len, elt) => attribute(a_y2, len, elt);
let x: t(Length.t, [> | `X], 'b, 'c) =
  (len, elt) => attribute(a_x, len, elt);
let y: t(Length.t, [> | `Y], 'b, 'c) =
  (len, elt) => attribute(a_y, len, elt);
let height: t(Length.t, [> | `Height], 'b, 'c) =
  (len, elt) => attribute(a_height, len, elt);
let width: t(Length.t, [> | `Width], 'b, 'c) =
  (len, elt) => attribute(a_width, len, elt);
let stroke_width: t(Length.t, [> | `Stroke_Width], 'b, 'c) =
  (len, elt) => attribute(a_stroke_width, len, elt);
let stroke_linecap: t(Shape.linecap, 'a, 'b, 'c) =
  (cap, elt) => attribute(a_stroke_linecap, cap, elt);
let stroke_dasharray: t(list(Length.t), 'a, 'b, 'c) =
  (lens, elt) => attribute(a_stroke_dasharray, lens, elt);

let fill: t(Color.t, [> | `Fill], 'b, 'c) =
  (color, elt) => attribute(a_fill, color, elt);
let stroke: t(Color.t, [> | `Stroke], 'b, 'c) =
  (color, elt) => attribute(a_stroke, color, elt);

let d: t(string, [> | `D], 'b, 'c) =
  (spec, elt) => attribute(a_d, spec, elt);
