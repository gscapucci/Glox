import gleam/int
import gleam/string

pub fn sub_string(str: String, index1: Int, index2: Int) -> String {
  let size = { index2 - index1 } |> int.absolute_value
  let str = str |> string.drop_left(int.min(index1, index2))
  str |> string.drop_right(str |> string.length |> int.subtract(size))
}
