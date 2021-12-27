open Bonsai_web;
open Sexplib.Std;

open Prelude;

module Model = {
  [@deriving sexp]
  type t = unit;
  let equal = (==);
  let default: t = ();
};

let lines_drawing = {
  open Drawing;
  let myline = (x1', y1', x2', y2', stroke_width', stroke') =>
    line
    |> x1(int(x1'))
    |> y1(int(y1'))
    |> x2(int(x2'))
    |> y2(int(y2'))
    |> stroke_width(float(stroke_width'))
    |> stroke(stroke');
  [
    myline(5, 10, 99, 30, 0.5, red),
    myline(5, 30, 99, 50, 1.0, red),
    myline(5, 50, 99, 70, 2.5, red),
    myline(5, 70, 99, 90, 4.0, blue),
  ]
  |> html_of_list;
};

let dashed_lines_drawing = {
  open Drawing;
  let myline = (x1', y1', x2', y2', stroke') =>
    line
    |> x1(int(x1'))
    |> y1(int(y1'))
    |> x2(int(x2'))
    |> y2(int(y2'))
    |> stroke(stroke')
    |> stroke_width(int(25));
  [
    myline(15, 15, 140, 135, blue) |> stroke_linecap(`Round),
    myline(15, 15, 140, 135, aqua) |> stroke_dasharray(ints([8, 3, 2, 18])),
    myline(15, 155, 160, 60, blue),
    myline(15, 155, 160, 60, orange) |> stroke_dasharray(ints([8, 3, 2])),
  ]
  |> html_of_list;
};

let rectangles_drawing = {
  open Drawing;
  let myrect = (x', y', height', width', fill') =>
    rect
    |> x(int(x'))
    |> y(int(y'))
    |> height(int(height'))
    |> width(int(width'))
    |> fill(fill')
    |> stroke(black)
    |> stroke_width(int(2));
  [
    myrect(62, 25, 110, 16, rgb(percent(100), percent(50), percent(50))),
    myrect(35, 35, 30, 50, red),
    myrect(5, 60, 30, 50, hex(0xf88)),
    myrect(25, 70, 30, 50, hex(0xff8888)),
    myrect(65, 60, 30, 50, hex(0xeac)),
    myrect(85, 70, 30, 50, hex(0xeeaacc)),
    myrect(60, 95, 30, 50, rgb(int(255), int(0), int(0))),
  ]
  |> html_of_list;
};

let circles_drawing =
  Drawing.(
    [
      circle |> cx(int(80)) |> cy(int(50)) |> r(int(40)),
      circle |> cx(int(80)) |> cy(int(110)) |> r(int(40)) |> fill(red),
      circle
      |> cx(int(80))
      |> cy(int(170))
      |> r(int(40))
      |> fill(yellow)
      |> stroke(blue),
      circle
      |> cx(int(80))
      |> cy(int(160))
      |> r(int(20))
      |> fill(red)
      |> stroke(black)
      |> stroke_width(int(10)),
      circle
      |> cx(int(140))
      |> cy(int(110))
      |> r(int(60))
      |> fill(none)
      |> stroke(hex(0x579))
      |> stroke_width(int(30))
      |> stroke_dasharray(ints([3, 5, 8, 13])),
    ]
    |> html_of_list
  );

let ellipses_drawing = {
  open Drawing;
  let myellipse = (cx', cy', rx', ry', fill', stroke', stroke_width') =>
    ellipse
    |> cx(int(cx'))
    |> cy(int(cy'))
    |> rx(int(rx'))
    |> ry(int(ry'))
    |> fill(fill')
    |> stroke(stroke')
    |> stroke_width(int(stroke_width'));
  [
    ellipse
    |> cx(int(80))
    |> cy(int(110))
    |> rx(int(75))
    |> ry(int(105))
    |> fill(hex(0x538)),
    myellipse(80, 110, 60, 40, black, red, 25),
    myellipse(80, 110, 35, 20, hex(0x538), yellow, 25),
    myellipse(80, 50, 40, 30, red, black, 25),
    myellipse(80, 50, 30, 20, orange, red, 10),
    myellipse(80, 170, 40, 30, yellow, orange, 25),
    myellipse(80, 170, 30, 20, red, black, 10),
  ]
  |> html_of_list;
};

let path_drawing =
  Drawing.(
    path |> stroke(black) |> d("M 100 100 L 200 200 100 150") |> html_of_t
  );

let path_api_drawing =
  Drawing.(
    Path.(
      [(100, 100), (200, 200), (100, 150)]
      |> of_int_pairs
      |> draw
      |> stroke(black)
      |> html_of_t
    )
  );

let _: Start.Handle.t(_) =
  Start.start_standalone(
    ~initial_input=(),
    ~bind_to_element_with_id="container",
    Bonsai.const(path_api_drawing),
  );
