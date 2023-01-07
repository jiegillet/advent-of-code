import gleam/erlang/file
import gleam/string
import gleam/function
import gleam/list
import gleam/map.{Map}
import gleam/order
import gleam/io
import gleam/bool
import gleam/int
import gleam/option.{Some}
import gleam/result
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day5.txt")

  solve_part1(input)
  |> io.println

  solve_part2(input)
  |> io.println
}

pub fn solve_part1(input: String) -> String {
  assert [state, moves] = string.split(input, "\n\n")
  let initial_state = get_state(state)

  moves
  |> string.split("\n")
  |> list.map(nibble.run(_, move_parser()))
  |> result.values
  |> list.fold(from: initial_state, with: move_crate_9000)
  |> get_top()
}

pub fn solve_part2(input: String) -> String {
  assert [state, moves] = string.split(input, "\n\n")
  let initial_state = get_state(state)

  moves
  |> string.split("\n")
  |> list.map(nibble.run(_, move_parser()))
  |> result.values
  |> list.fold(from: initial_state, with: move_crate_9001)
  |> get_top()
}

type State =
  Map(Int, List(String))

fn get_state(input: String) -> State {
  input
  |> string.split("\n")
  |> list.map(string.to_graphemes)
  |> list.transpose
  |> list.map(list.filter(_, is_crate))
  |> list.filter(fn(list) { !list.is_empty(list) })
  |> list.index_map(fn(index, crates) { #(index + 1, crates) })
  |> map.from_list
}

fn is_crate(grapheme: String) -> Bool {
  { string.compare(grapheme, "A") == order.Lt }
  |> bool.or(string.compare(grapheme, "Z") == order.Gt)
  |> bool.negate
}

fn get_top(state: State) -> String {
  state
  |> map.to_list
  |> list.sort(by: fn(a, b) { int.compare(a.0, b.0) })
  |> list.map(fn(a) { a.1 })
  |> list.map(list.first)
  |> result.values
  |> string.join("")
}

fn move_crate_9000(state: State, move: Move) -> State {
  assert Ok(from_crates) = map.get(state, move.from)
  let crates =
    from_crates
    |> list.take(move.crates)
    |> list.reverse

  state
  |> map.update(
    update: move.from,
    with: fn(_) { list.drop(from_crates, move.crates) },
  )
  |> map.update(
    update: move.to,
    with: fn(optional_list) {
      assert Some(to_crates) = optional_list
      list.append(crates, to_crates)
    },
  )
}

fn move_crate_9001(state: State, move: Move) -> State {
  assert Ok(from_crates) = map.get(state, move.from)
  let crates = list.take(from_crates, move.crates)

  state
  |> map.update(
    update: move.from,
    with: function.constant(list.drop(from_crates, move.crates)),
  )
  |> map.update(
    update: move.to,
    with: fn(optional_list) {
      assert Some(to_crates) = optional_list
      list.append(crates, to_crates)
    },
  )
}

type Move {
  Move(crates: Int, from: Int, to: Int)
}

fn move_parser() -> Parser(Move, ctx) {
  nibble.succeed(function.curry3(Move))
  |> nibble.drop(nibble.string("move "))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string(" from "))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string(" to "))
  |> nibble.keep(nibble.int())
}
