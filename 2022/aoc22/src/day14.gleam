import gleam/erlang/file
import gleam/list
import gleam/io
import gleam/string
import gleam/int
import gleam/map.{Map}
import gleam/result
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day14.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  let cave = build_cave(input)
  let deepest =
    cave
    |> map.keys
    |> list.map(fn(pos) { pos.1 })
    |> list.fold(from: 0, with: int.max)

  pour_sand(cave, deepest, start)
}

pub fn solve_part2(input: String) -> Int {
  let cave = build_cave(input)
  let deepest =
    cave
    |> map.keys
    |> list.map(fn(pos) { pos.1 })
    |> list.fold(from: 0, with: int.max)

  let deepest = deepest + 2

  let floor =
    list.range(500 - deepest, 500 + deepest)
    |> list.map(fn(i) { #(#(i, deepest), Rock) })
    |> map.from_list

  let cave = map.merge(cave, floor)

  pour_sand(cave, deepest, start)
}

type Position =
  #(Int, Int)

const start: Position = #(500, 0)

type Wall =
  List(Position)

type Element {
  Rock
  Sand
}

type Cave =
  Map(Position, Element)

fn build_cave(input: String) -> Cave {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(nibble.run(_, wall_parser()))
  |> result.values
  |> list.flatten
  |> list.map(fn(pos) { #(pos, Rock) })
  |> map.from_list
}

fn pour_sand(cave: Cave, deepest: Int, sand: Position) -> Int {
  let #(i, j) = sand
  let current = map.get(cave, sand)

  case Nil {
    _ if j >= deepest || current == Ok(Sand) ->
      cave
      |> map.values
      |> list.filter(fn(el) { el == Sand })
      |> list.length
    _ -> {
      let down = #(i, j + 1)
      let down_left = #(i - 1, j + 1)
      let down_right = #(i + 1, j + 1)
      case
        map.get(cave, down),
        map.get(cave, down_left),
        map.get(cave, down_right)
      {
        Error(Nil), _, _ -> pour_sand(cave, deepest, down)
        _, Error(Nil), _ -> pour_sand(cave, deepest, down_left)
        _, _, Error(Nil) -> pour_sand(cave, deepest, down_right)
        _, _, _ -> pour_sand(map.insert(cave, sand, Sand), deepest, start)
      }
    }
  }
}

fn wall_parser() -> Parser(Wall, ctx) {
  let build_wall = fn(corners) {
    corners
    |> list.window_by_2()
    |> list.flat_map(fn(pair) {
      let #(#(ax, ay), #(bx, by)) = pair
      case Nil {
        _ if ax == bx ->
          list.range(ay, by)
          |> list.map(fn(y) { #(ax, y) })
        _ if ay == by ->
          list.range(ax, bx)
          |> list.map(fn(x) { #(x, ay) })
      }
    })
  }

  nibble.many(position_parser(), nibble.string(" -> "))
  |> nibble.map(build_wall)
}

fn position_parser() -> Parser(Position, ctx) {
  nibble.succeed(fn(i) { fn(j) { #(i, j) } })
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string(","))
  |> nibble.keep(nibble.int())
}
