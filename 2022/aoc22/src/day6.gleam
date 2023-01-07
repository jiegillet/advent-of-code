import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/set
import gleam/io
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day6.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  find_starter(input, 4)
}

pub fn solve_part2(input: String) -> Int {
  find_starter(input, 14)
}

fn find_starter(input: String, num_characters: Int) -> Int {
  let non_starters =
    input
    |> string.to_graphemes
    |> list.window(by: num_characters)
    |> list.take_while(fn(message) {
      set.size(set.from_list(message)) < num_characters
    })
    |> list.length

  non_starters + num_characters
}
