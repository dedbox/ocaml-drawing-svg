include Attribute;
include Color;
include Shape;
include Length;
include Drawing;

module IntPath =
  Path.Make({
    type t = int;
    let string_of_t = Int.to_string;
  });

module FloatPath =
  Path.Make({
    type t = float;
    let string_of_t = Float.to_string;
  });

module Path = {
  module Coordinate = {
    type t =
      | I(int)
      | F(float);
    let string_of_t =
      fun
      | I(x) => Int.to_string(x)
      | F(x) => Float.to_string(x);
  };
  include Path.Make(Coordinate);
};
