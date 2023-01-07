import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/set
import gleam/io
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day3.txt")

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
  |> list.map(shared_item)
  |> list.map(priority)
  |> int.sum
}

pub fn solve_part2(input: String) -> Int {
  input
  |> string.trim
  |> string.split("\n")
  |> list.sized_chunk(into: 3)
  |> list.map(find_badge)
  |> list.map(priority)
  |> int.sum
}

fn shared_item(rucksack: String) -> String {
  let size = string.length(rucksack)
  let first_items =
    rucksack
    |> string.drop_right(up_to: size / 2)
    |> string.to_graphemes
    |> set.from_list
  let second_items =
    rucksack
    |> string.drop_left(up_to: size / 2)
    |> string.to_graphemes
    |> set.from_list

  assert [item] = set.to_list(set.intersection(first_items, second_items))
  item
}

fn find_badge(group: List(String)) -> String {
  let sets =
    group
    |> list.map(string.to_graphemes)
    |> list.map(set.from_list)

  assert Ok(badge_set) = list.reduce(over: sets, with: set.intersection)
  assert [badge] = set.to_list(badge_set)
  badge
}

fn priority(letter: String) -> Int {
  assert <<x:int>> = <<letter:utf8>>
  assert <<a, z, aa, zz:int>> = <<"azAZ":utf8>>
  case x {
    x if a <= x && x <= z -> x - a + 1
    x if aa <= x && x <= zz -> x - aa + 27
  }
}
