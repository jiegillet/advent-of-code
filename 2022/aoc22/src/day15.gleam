import gleam/erlang/file
import gleam/list
import gleam/iterator.{Iterator}
import gleam/io
import gleam/string
import gleam/function
import gleam/int
import gleam/result
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day15.txt")

  solve_part1(input, 2_000_000)
  |> int.to_string
  |> io.println

  solve_part2(input, 4_000_000)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String, row: Int) -> Int {
  let sensors =
    input
    |> string.trim
    |> string.split("\n")
    |> list.map(nibble.run(_, sensor_parser()))
    |> result.values

  let beacons_in_row =
    sensors
    |> list.map(fn(sensor) { sensor.beacon })
    |> list.filter(fn(beacon) { beacon.1 == row })
    |> list.unique
    |> list.length

  let count_ranges =
    sensors
    |> list.map(detection_at_row(_, row))
    |> result.values
    |> list.sort(by: fn(a, b) { int.compare(a.0, b.0) })
    |> fuse_ranges
    |> list.map(fn(range) { range.1 - range.0 + 1 })
    |> int.sum

  count_ranges - beacons_in_row
}

pub fn solve_part2(input: String, max_coordinate: Int) -> Int {
  let sensors =
    input
    |> string.trim
    |> string.split("\n")
    |> list.map(nibble.run(_, sensor_parser()))
    |> result.values

  assert Ok(#(x, y)) =
    sensors
    |> iterator.from_list
    |> iterator.flat_map(find_boundaries)
    |> iterator.find(hidden_beacon(_, sensors, max_coordinate))

  x * 4_000_000 + y
}

type Position =
  #(Int, Int)

type Range =
  #(Int, Int)

type Sensor {
  Sensor(position: Position, beacon: Position)
}

fn detection_at_row(sensor: Sensor, row: Int) -> Result(Range, Nil) {
  let Sensor(#(sx, sy), _) = sensor
  let dist = distance(sensor)

  case dist - int.absolute_value(sy - row) {
    d if d < 0 -> Error(Nil)
    d -> Ok(#(sx - d, sx + d))
  }
}

fn distance(sensor: Sensor) -> Int {
  let Sensor(#(sx, sy), #(bx, by)) = sensor
  int.absolute_value(sx - bx) + int.absolute_value(sy - by)
}

fn fuse_ranges(ranges: List(Range)) -> List(Range) {
  case ranges {
    [] | [_] -> ranges
    [a, b, ..rest] if a.1 < b.0 -> [a, ..fuse_ranges([b, ..rest])]
    [a, b, ..rest] -> fuse_ranges([#(a.0, int.max(a.1, b.1)), ..rest])
  }
}

fn find_boundaries(sensor: Sensor) -> Iterator(Position) {
  let Sensor(#(sx, sy), _) = sensor
  let dist = distance(sensor)

  iterator.range(0, dist)
  |> iterator.flat_map(fn(k) {
    iterator.from_list([
      #(sx + k, sy + dist + 1 - k),
      #(sx - k, sy - dist - 1 + k),
      #(sx + dist + 1 - k, sy - k),
      #(sx - dist - 1 + k, sy + k),
    ])
  })
}

fn hidden_beacon(
  position: Position,
  sensors: List(Sensor),
  max_coordinate: Int,
) -> Bool {
  let #(x, y) = position

  0 <= x && x <= max_coordinate && 0 <= y && y <= max_coordinate && list.all(
    sensors,
    is_outside(position, _),
  )
}

fn is_outside(position: Position, sensor: Sensor) -> Bool {
  let #(x, y) = position
  let Sensor(#(sx, sy), _) = sensor
  let dist = distance(sensor)

  int.absolute_value(sx - x) + int.absolute_value(sy - y) > dist
}

fn sensor_parser() -> Parser(Sensor, ctx) {
  let to_sensor = fn(sx, sy, bx, by) { Sensor(#(sx, sy), #(bx, by)) }

  nibble.succeed(function.curry4(to_sensor))
  |> nibble.drop(nibble.string("Sensor at x="))
  |> nibble.keep(integer())
  |> nibble.drop(nibble.string(", y="))
  |> nibble.keep(integer())
  |> nibble.drop(nibble.string(": closest beacon is at x="))
  |> nibble.keep(integer())
  |> nibble.drop(nibble.string(", y="))
  |> nibble.keep(integer())
}

fn integer() -> Parser(Int, ctx) {
  nibble.one_of([
    nibble.int(),
    nibble.succeed(int.negate)
    |> nibble.drop(nibble.string("-"))
    |> nibble.keep(nibble.int()),
  ])
}
