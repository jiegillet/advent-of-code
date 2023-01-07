import gleam/erlang/file
import gleam/string
import gleam/function
import gleam/list
import gleam/map.{Map}
import gleam/io
import gleam/int
import gleam/set.{Set}

pub fn run() {
  assert Ok(input) = file.read("inputs/day12.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  let mountain = parse_mountain(input)
  let params = Params(start: "S", end: "E", can_move_to: can_move_to)
  steps_to_top(mountain, params)
}

pub fn solve_part2(input: String) -> Int {
  let mountain = parse_mountain(input)
  let params =
    Params(start: "E", end: "a", can_move_to: function.flip(can_move_to))
  steps_to_top(mountain, params)
}

type Position =
  #(Int, Int)

type Mountain =
  Map(Position, String)

type Params {
  Params(start: String, end: String, can_move_to: fn(String, String) -> Bool)
}

fn parse_mountain(input: String) -> Mountain {
  input
  |> string.trim
  |> string.split("\n")
  |> list.index_map(fn(i, row) {
    row
    |> string.to_graphemes
    |> list.index_map(fn(j, height) { #(#(i, j), height) })
  })
  |> list.flatten
  |> map.from_list
}

fn steps_to_top(mountain: Mountain, params: Params) -> Int {
  assert [start] =
    mountain
    |> map.filter(fn(_, height) { height == params.start })
    |> map.keys
  let explored = set.from_list([start])
  let exploring = [#(start, 0)]
  explore(mountain, exploring, explored, params)
}

fn explore(
  mountain: Mountain,
  exploring: List(#(Position, Int)),
  explored: Set(Position),
  params: Params,
) -> Int {
  assert [#(position, steps), ..to_explore] = exploring
  let end = params.end
  case map.get(mountain, position) {
    Ok(height) if height == end -> steps
    Ok(height) -> {
      let #(i, j) = position
      let neighbors =
        [#(i + 1, j), #(i - 1, j), #(i, j + 1), #(i, j - 1)]
        |> list.filter(fn(pos) { !set.contains(explored, pos) })
        |> list.filter(fn(pos) {
          case map.get(mountain, pos) {
            Ok(next_height) -> params.can_move_to(height, next_height)
            Error(Nil) -> False
          }
        })
      let explored =
        list.fold(over: neighbors, from: explored, with: set.insert)
      let neighbors = list.map(neighbors, fn(pos) { #(pos, steps + 1) })
      let exploring = list.append(to_explore, neighbors)
      explore(mountain, exploring, explored, params)
    }
    Error(Nil) -> -1
  }
}

fn can_move_to(current_height: String, next_height: String) -> Bool {
  let check_height = fn(height) {
    case height {
      "S" -> "a"
      "E" -> "z"
      _ -> height
    }
  }

  let current_height = check_height(current_height)
  let next_height = check_height(next_height)
  let <<current_height:int>> = <<current_height:utf8>>
  let <<next_height:int>> = <<next_height:utf8>>
  next_height <= current_height + 1
}
