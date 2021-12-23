open Virtual_dom.Tyxml;
open Svg;

module T = Svg_types;

type t('a, 'b, 'c) = (list(attrib('a)), star('a, 'b, 'c));

let to_svg: t('a, 'b, 'c) => elt('c) = ((a, shape)) => shape(~a, []);

let to_html = (drawing: t('a, 'b, 'c)): Xml.elt =>
  Html.svg([drawing |> to_svg]) |> Html.toelt;

let circle: t(T.circle_attr, T.circle_content, T.circle) = ([], circle);

let line: t(T.line_attr, T.line_content, T.line) = ([], line);

// Length

type length = Svg_types.Unit.length;

let em = (x: float): length => (x, Some(`Em));
let ex = (x: float): length => (x, Some(`Ex));
let px = (x: float): length => (x, Some(`Px));
let inches = (x: float): length => (x, Some(`In));
let cm = (x: float): length => (x, Some(`Cm));
let mm = (x: float): length => (x, Some(`Mm));
let pt = (x: float): length => (x, Some(`Pt));
let pc = (x: float): length => (x, Some(`Pc));
let percent = (x: float): length => (x, Some(`Percent));

// Color

type color = T.paint;

let named_color = (name: string): color => `Color((name, None));
let no_color: color = `None;
let current_color: color = `CurrentColor;
let red: color = named_color("red");
let green: color = named_color("green");
let blue: color = named_color("blue");
let cyan: color = named_color("cyan");
let magenta: color = named_color("magenta");
let yellow: color = named_color("yellow");
let black: color = named_color("black");
let white: color = named_color("white");

// Attribute

type attribute('x, 'a, 'b, 'c) = ('x, t('a, 'b, 'c)) => t('a, 'b, 'c);

let attribute =
    (f: 'x => attrib('a), x: 'x, (a, shape): t('a, 'b, 'c)): t('a, 'b, 'c) => (
  [f(x), ...a],
  shape,
);

let r: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_r, len, elt);
let cx: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_cx, len, elt);
let cy: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_cy, len, elt);
let x1: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_x1, len, elt);
let y1: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_y1, len, elt);
let x2: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_x2, len, elt);
let y2: attribute(length, 'a, 'b, 'c) =
  (len, elt) => attribute(a_y2, len, elt);

let fill: attribute(color, 'a, 'b, 'c) =
  (color, elt) => attribute(a_fill, color, elt);
let stroke: attribute(color, 'a, 'b, 'c) =
  (color, elt) => attribute(a_stroke, color, elt);
