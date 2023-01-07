import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/io
import gleam/int
import gleam/result

pub fn run() {
  try input = file.read("inputs/day1.txt")

  solve_part1(input)
  |> io.debug

  solve_part2(input)
  |> io.debug

  Ok(Nil)
}

pub fn solve_part1(input) {
  input
  |> string.split("\n\n")
  |> list.map(block_sum)
  |> list.fold(0, int.max)
}

pub fn solve_part2(input) {
  input
  |> string.split("\n\n")
  |> list.map(block_sum)
  |> list.sort(int.compare)
  |> list.reverse
  |> list.take(3)
  |> int.sum
}

fn block_sum(group) {
  group
  |> string.split("\n")
  |> list.map(int.parse)
  |> result.values
  |> int.sum
}
