module T = Svg_types;

open Virtual_dom.Tyxml.Svg;

type line = Drawing.t(T.line_attr, T.line_content, T.line);
type rect = Drawing.t(T.rect_attr, T.rect_content, T.rect);
type circle = Drawing.t(T.circle_attr, T.circle_content, T.circle);
type ellipse = Drawing.t(T.ellipse_attr, T.ellipse_content, T.ellipse);
type path = Drawing.t(T.path_attr, T.path_content, T.path);

type linecap = [ | `Butt | `Round | `Square];

let line: line = ([], line, []);
let rect: rect = ([], rect, []);
let circle: circle = ([], circle, []);
let ellipse: ellipse = ([], ellipse, []);
let path: path = ([], path, []);
