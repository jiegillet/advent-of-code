import gleam/erlang/file
import gleam/function
import gleam/string
import gleam/list
import gleam/io
import gleam/int
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day4.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  input
  |> string.trim
  |> string.split("\n")
  |> list.filter(has_full_overlap)
  |> list.length
}

pub fn solve_part2(input: String) -> Int {
  input
  |> string.trim
  |> string.split("\n")
  |> list.filter(has_any_overlap)
  |> list.length
}

fn has_full_overlap(row) {
  assert Ok(#(a, b)) = nibble.run(row, row_parser())
  a.from <= b.from && b.to <= a.to || b.from <= a.from && a.to <= b.to
}

fn has_any_overlap(row) {
  assert Ok(#(a, b)) = nibble.run(row, row_parser())
  a.from <= b.from && b.from <= a.to || a.from <= b.to && b.to <= a.to || b.from <= a.from && a.to <= b.to
}

type Section {
  Section(from: Int, to: Int)
}

fn section_parser() -> Parser(Section, ctx) {
  nibble.succeed(function.curry2(Section))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.grapheme("-"))
  |> nibble.keep(nibble.int())
}

fn row_parser() -> Parser(#(Section, Section), ctx) {
  nibble.succeed(function.curry2(fn(x, y) { #(x, y) }))
  |> nibble.keep(section_parser())
  |> nibble.drop(nibble.grapheme(","))
  |> nibble.keep(section_parser())
}
