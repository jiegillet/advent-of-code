import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/io
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day10.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  let initial_state = State(x: 1, cycle: 1)

  input
  |> string.trim
  |> string.split("\n")
  |> list.map(parse_op)
  |> list.fold(from: [initial_state], with: eval_op)
  |> list.filter(fn(state) { { state.cycle - 20 } % 40 == 0 })
  |> list.map(signal_strength)
  |> int.sum
}

pub fn solve_part2(input: String) -> String {
  let initial_state = State(x: 1, cycle: 1)

  input
  |> string.trim
  |> string.split("\n")
  |> list.map(parse_op)
  |> list.fold(from: [initial_state], with: eval_op)
  |> list.drop(1)
  |> list.reverse
  |> list.map(draw_pixel)
  |> list.sized_chunk(40)
  |> list.map(string.join(_, ""))
  |> string.join("\n")
}

type State {
  State(x: Int, cycle: Int)
}

type Op {
  Addx(Int)
  Noop
}

fn parse_op(input: String) -> Op {
  case string.split(input, " ") {
    ["noop"] -> Noop
    ["addx", string_value] -> {
      assert Ok(value) = int.parse(string_value)
      Addx(value)
    }
  }
}

fn eval_op(states: List(State), op: Op) -> List(State) {
  assert [state, ..] = states
  let State(x: x, cycle: cycle) = state
  case op {
    Noop -> [State(x: x, cycle: cycle + 1), ..states]
    Addx(value) -> [
      State(x: x + value, cycle: cycle + 2),
      State(x: x, cycle: cycle + 1),
      ..states
    ]
  }
}

fn signal_strength(state: State) -> Int {
  let State(x: x, cycle: cycle) = state
  x * cycle
}

fn draw_pixel(state: State) -> String {
  let State(x: x, cycle: cycle) = state
  case int.absolute_value(x - { cycle - 1 } % 40) <= 1 {
    True -> "#"
    False -> " "
  }
}
