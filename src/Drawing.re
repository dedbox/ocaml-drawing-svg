open Virtual_dom.Tyxml;
open Svg;

type t('a, 'b, 'c) = (
  list(attrib('a)),
  star('a, 'b, 'c),
  list(elt('b)),
);

let svg_of_t: t('a, 'b, 'c) => elt('c) =
  ((a, shape, elts)) => shape(~a, elts);

let html_of_list = (drawings: list(t('a, 'b, 'c))): Xml.elt =>
  Html.svg(drawings |> List.map(svg_of_t)) |> Html.toelt;

let html_of_t = (drawing: t('a, 'b, 'c)): Xml.elt =>
  html_of_list([drawing]);
