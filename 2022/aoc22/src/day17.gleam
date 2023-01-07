import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/io
import gleam/set.{Set}
import gleam/iterator
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day17.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  let directions =
    input
    |> string.trim
    |> to_directions

  let state = State(0, #(2, 4), set.from_list([#(3, 0)]), 0)

  assert Ok(State(height: height, ..)) =
    directions
    |> iterator.from_list
    |> iterator.cycle
    |> iterator.scan(from: state, with: drop_block)
    |> iterator.find(fn(state) { state.round == 2022 })

  height
}

pub fn solve_part2(input: String) -> Int {
  let directions =
    input
    |> string.trim
    |> to_directions

  let state = State(0, #(2, 4), set.from_list([#(3, 0)]), 0)

  // after a round, it's always Minus piece, 1 right, 2 down from spawning 
  let state1 = list.fold(directions, state, drop_block)
  let state2 = list.fold(directions, state1, drop_block)

  let round_cycle = state2.round - state1.round
  let height_cycle = state2.height - state1.height

  let rounds = 1_000_000_000_000
  let cycles = { rounds - state1.round } / round_cycle
  let left = { rounds - state1.round } % round_cycle

  assert Ok(State(height: height, ..)) =
    directions
    |> iterator.from_list
    |> iterator.cycle
    |> iterator.scan(from: state2, with: drop_block)
    |> iterator.find(fn(state) { state.round == state2.round + left })

  state1.height + cycles * height_cycle + height - state2.height
}

type Direction {
  Left
  Right
}

fn to_directions(input: String) -> List(Direction) {
  input
  |> string.to_graphemes
  |> list.map(fn(char) {
    case char {
      "<" -> Left
      ">" -> Right
    }
  })
}

type Position =
  #(Int, Int)

type Pattern =
  Set(Position)

type Rock {
  Minus
  Plus
  Corner
  Pipe
  Square
}

fn rock_order(round: Int) -> Rock {
  case round % 5 {
    0 -> Minus
    1 -> Plus
    2 -> Corner
    3 -> Pipe
    4 -> Square
  }
}

type State {
  State(round: Int, offset: Position, pattern: Pattern, height: Int)
}

fn solidify_rock(rock: Rock, offset: Position) -> Pattern {
  case rock {
    Minus -> [#(0, 0), #(1, 0), #(2, 0), #(3, 0)]
    Plus -> [#(1, 0), #(0, -1), #(1, -1), #(2, -1), #(1, -2)]
    Corner -> [#(2, 0), #(2, -1), #(0, -2), #(1, -2), #(2, -2)]
    Pipe -> [#(0, 0), #(0, -1), #(0, -2), #(0, -3)]
    Square -> [#(0, 0), #(0, -1), #(1, 0), #(1, -1)]
  }
  |> list.map(fn(pos) { #(pos.0 + offset.0, pos.1 + offset.1) })
  |> set.from_list
}

fn blocked_in_direction(
  rock: Rock,
  offset: Position,
  direction: Direction,
  state: Pattern,
) -> Bool {
  let #(x, y) = offset
  let blocked = case direction, rock {
    Left, _ if x == 0 -> True
    Right, Minus if x == 3 -> True
    Right, Plus if x == 4 -> True
    Right, Corner if x == 4 -> True
    Right, Pipe if x == 6 -> True
    Right, Square if x == 5 -> True
    _, _ -> False
  }

  blocked || case direction, rock {
    Left, Minus -> [#(-1, 0)]
    Left, Plus -> [#(0, 0), #(-1, -1), #(0, -2)]
    Left, Corner -> [#(1, 0), #(1, -1), #(-1, -2)]
    Left, Pipe -> [#(-1, 0), #(-1, -1), #(-1, -2), #(-1, -3)]
    Left, Square -> [#(-1, 0), #(-1, -1)]
    Right, Minus -> [#(4, 0)]
    Right, Plus -> [#(2, 0), #(3, -1), #(2, -2)]
    Right, Corner -> [#(3, 0), #(3, -1), #(3, -2)]
    Right, Pipe -> [#(1, 0), #(1, -1), #(1, -2), #(1, -3)]
    Right, Square -> [#(2, 0), #(2, -1)]
  }
  |> list.map(fn(pos) { #(pos.0 + x, pos.1 + y) })
  |> list.any(set.contains(state, _))
}

fn blocked_down(rock: Rock, offset: Position, state: Pattern) -> Bool {
  let #(x, y) = offset
  case rock {
    Minus -> [#(0, -1), #(1, -1), #(2, -1), #(3, -1)]
    Plus -> [#(0, -2), #(1, -3), #(2, -2)]
    Corner -> [#(0, -3), #(1, -3), #(2, -3)]
    Pipe -> [#(0, -4)]
    Square -> [#(0, -2), #(1, -2)]
  }
  |> list.map(fn(pos) { #(pos.0 + x, pos.1 + y) })
  |> list.any(set.contains(state, _))
}

fn drop_block(state: State, direction: Direction) -> State {
  let State(round, offset, pattern, height) = state
  let rock = rock_order(round)
  let #(x, y) = offset
  let offset = case
    blocked_in_direction(rock, offset, direction, pattern),
    direction
  {
    True, _ -> offset
    False, Left -> #(x - 1, y)
    False, Right -> #(x + 1, y)
  }
  let #(x, y) = offset

  case blocked_down(rock, offset, pattern) {
    True -> {
      let rock_pattern = solidify_rock(rock, offset)
      let height = int.max(height, max_height(rock_pattern))
      let pattern = set.union(pattern, rock_pattern)
      State(round + 1, new_offset(round + 1, height), pattern, height)
    }

    False -> State(round, #(x, y - 1), pattern, height)
  }
}

fn new_offset(round: Int, height: Int) -> Position {
  let rock = rock_order(round)
  let y_offset = case rock {
    Minus -> 4
    Plus -> 6
    Corner -> 6
    Pipe -> 7
    Square -> 5
  }

  #(2, height + y_offset)
}

fn max_height(pattern: Pattern) -> Int {
  pattern
  |> set.to_list
  |> list.map(fn(pos) { pos.1 })
  |> list.fold(from: 0, with: int.max)
}
