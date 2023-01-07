import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/io
import gleam/int
import gleam/result
import gleam/order.{Eq, Gt, Lt, Order}
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day13.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  input
  |> string.split("\n\n")
  |> list.map(nibble.run(_, pair_list_parser()))
  |> result.values
  |> list.index_map(fn(index, lists) { #(index, lists) })
  |> list.filter_map(fn(pair) {
    let #(index, #(a, b)) = pair
    case compare(a, b) {
      Gt -> Error(Nil)
      _ -> Ok(index + 1)
    }
  })
  |> int.sum
}

pub fn solve_part2(input: String) -> Int {
  assert Ok(lists) = nibble.run(input, nested_list_list_parser())
  let two = Nested([Nested([Value(2)])])
  let six = Nested([Nested([Value(6)])])

  [two, six, ..lists]
  |> list.sort(compare)
  |> list.index_map(fn(index, list) { #(index, list) })
  |> list.filter_map(fn(pair) {
    let #(index, a) = pair
    case a {
      _ if a == two || a == six -> Ok(index + 1)
      _ -> Error(Nil)
    }
  })
  |> int.product
}

type NestedList {
  Nested(List(NestedList))
  Value(Int)
}

fn compare(a: NestedList, b: NestedList) -> Order {
  case a, b {
    Value(a), Value(b) -> int.compare(a, b)
    Value(_), Nested(_) -> compare(Nested([a]), b)
    Nested(_), Value(_) -> compare(a, Nested([b]))
    Nested([]), Nested([]) -> Eq
    Nested([]), Nested(_) -> Lt
    Nested(_), Nested([]) -> Gt
    Nested([a, ..aa]), Nested([b, ..bb]) ->
      case compare(a, b) {
        Eq -> compare(Nested(aa), Nested(bb))
        other -> other
      }
  }
}

fn pair_list_parser() -> Parser(#(NestedList, NestedList), ctx) {
  nibble.succeed(fn(a) { fn(b) { #(a, b) } })
  |> nibble.keep(nested_list_parser())
  |> nibble.drop(nibble.string("\n"))
  |> nibble.keep(nested_list_parser())
}

fn nested_list_list_parser() -> Parser(List(NestedList), ctx) {
  nibble.many(
    nibble.drop(
      nested_list_parser(),
      nibble.one_of([nibble.string("\n\n"), nibble.string("\n")]),
    ),
    nibble.succeed(Nil),
  )
}

fn nested_list_parser() -> Parser(NestedList, ctx) {
  nibble.one_of([
    nibble.map(nibble.int(), Value),
    nibble.succeed(Nested)
    |> nibble.drop(nibble.string("["))
    |> nibble.keep(nibble.many(
      nibble.lazy(fn() { nested_list_parser() }),
      nibble.string(","),
    ))
    |> nibble.drop(nibble.string("]")),
  ])
}
