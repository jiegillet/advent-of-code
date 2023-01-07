import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/set.{Set}
import gleam/io
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day9.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  solve_with_length(input, 2)
}

pub fn solve_part2(input: String) -> Int {
  solve_with_length(input, 10)
}

fn solve_with_length(input: String, n: Int) -> Int {
  let initial_state = State(list.repeat(#(0, 0), n), set.new())
  let State(history: history, ..) =
    input
    |> string.trim
    |> string.split("\n")
    |> list.flat_map(parse_move)
    |> list.fold(from: initial_state, with: move)
  set.size(history)
}

type State {
  State(rope: List(#(Int, Int)), history: Set(#(Int, Int)))
}

type Direction {
  Up
  Down
  Left
  Right
}

fn parse_move(input: String) -> List(Direction) {
  assert [dir, steps] = string.split(input, " ")
  assert Ok(steps) = int.parse(steps)
  case dir {
    "U" -> list.repeat(item: Up, times: steps)
    "D" -> list.repeat(item: Down, times: steps)
    "L" -> list.repeat(item: Left, times: steps)
    "R" -> list.repeat(item: Right, times: steps)
  }
}

fn move(state: State, dir: Direction) -> State {
  let State([head, ..tail], history) = state
  let new_head = move_head(dir, head)
  assert [last, ..] as rev_rope =
    list.fold(over: tail, from: [new_head], with: move_rope)
  let history = set.insert(history, last)
  State(list.reverse(rev_rope), history)
}

fn move_head(dir: Direction, position: #(Int, Int)) -> #(Int, Int) {
  let #(i, j) = position
  case dir {
    Up -> #(i + 1, j)
    Down -> #(i - 1, j)
    Left -> #(i, j - 1)
    Right -> #(i, j + 1)
  }
}

fn move_rope(rope: List(#(Int, Int)), knot: #(Int, Int)) -> List(#(Int, Int)) {
  assert [head, ..] = rope
  let new_tail = move_rope_element(head, knot)
  [new_tail, ..rope]
}

fn move_rope_element(head: #(Int, Int), tail: #(Int, Int)) -> #(Int, Int) {
  let #(i, j) = head
  let #(x, y) = tail
  let dx = i - x
  let dy = j - y
  case Nil {
    _ if x == i && dy == 2 -> #(x, y + 1)
    _ if x == i && dy == -2 -> #(x, y - 1)
    _ if y == j && dx == 2 -> #(x + 1, y)
    _ if y == j && dx == -2 -> #(x - 1, y)
    _ if { dx == 1 || dx == -1 } && dy == 2 -> #(i, y + 1)
    _ if { dx == 1 || dx == -1 } && dy == -2 -> #(i, y - 1)
    _ if { dy == 1 || dy == -1 } && dx == 2 -> #(x + 1, j)
    _ if { dy == 1 || dy == -1 } && dx == -2 -> #(x - 1, j)
    _ if dy == 2 && dx == 2 -> #(x + 1, y + 1)
    _ if dy == 2 && dx == -2 -> #(x - 1, y + 1)
    _ if dy == -2 && dx == 2 -> #(x + 1, y - 1)
    _ if dy == -2 && dx == -2 -> #(x - 1, y - 1)
    _ -> #(x, y)
  }
}
