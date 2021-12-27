type t('a) = ('a, list('a));

let string_of_t =
    (~sep: string=" ", string_of_elt: 'a => string, (x, xs): t('a)): string =>
  [x, ...xs] |> List.map(string_of_elt) |> String.concat(sep);

let init = (~tail: list('a)=[], x: 'a): t('a) => (x, tail);

let elements = ((x, xs): t('a)): list('a) => [x, ...xs];

let cons = (x: 'a, (y, ys): t('a)): t('a) => (x, [y, ...ys]);

let append = ((x, xs): t('a), (y, ys): t('a)): t('a) => (
  x,
  xs @ [y, ...ys],
);

let peek = ((x, xs): t('a)): ('a, list('a)) => (x, xs);

let map = (f: 'a => 'b, (x, xs): t('a)): t('b) => (
  f(x),
  xs |> List.map(f),
);
