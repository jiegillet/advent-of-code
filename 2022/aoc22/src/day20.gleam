import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/result
import gleam/map.{Map}
import gleam/io
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day20.txt")

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
  |> list.map(int.parse)
  |> result.values
  |> decrypt
}

pub fn solve_part2(input: String) -> Int {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(int.parse)
  |> result.values
  |> decrypt_v2
}

type Link(a) {
  Link(before: a, after: a)
}

type Circle =
  Map(#(Int, Int), Link(#(Int, Int)))

fn decrypt(numbers: List(Int)) -> Int {
  let numbers = list.index_map(numbers, fn(index, n) { #(index, n) })
  assert [a, b, ..] = numbers

  assert Ok(zero) = list.find(numbers, fn(pair) { pair.1 == 0 })

  let circle =
    numbers
    |> list.append([a, b])
    |> list.window(3)
    |> list.map(fn(nums) {
      assert [before, here, after] = nums
      #(here, Link(before, after))
    })
    |> map.from_list

  numbers
  |> list.fold(from: circle, with: move)
  |> find_coordinate(zero)
}

fn decrypt_v2(numbers: List(Int)) -> Int {
  let decryption = 811589153
  let numbers =
    list.index_map(numbers, fn(index, n) { #(index, decryption * n) })
  assert [a, b, ..] = numbers

  assert Ok(zero) = list.find(numbers, fn(pair) { pair.1 == 0 })

  let circle =
    numbers
    |> list.append([a, b])
    |> list.window(3)
    |> list.map(fn(nums) {
      assert [before, here, after] = nums
      #(here, Link(before, after))
    })
    |> map.from_list

  list.range(1, 10)
  |> list.fold(
    from: circle,
    with: fn(circle, _) { list.fold(over: numbers, from: circle, with: move) },
  )
  |> find_coordinate(zero)
}

fn move(circle: Circle, number: #(Int, Int)) -> Circle {
  let #(_, n) = number
  case n {
    _ if n <= 0 -> swap_left(n % { map.size(circle) - 1 }, number, circle)
    _ if n > 0 -> swap_right(n % { map.size(circle) - 1 }, number, circle)
  }
}

fn swap_left(steps: Int, number: #(Int, Int), circle: Circle) -> Circle {
  case steps {
    0 -> circle
    _ -> {
      // from: before_before before number after 
      // to:   before_before number before after 
      assert Ok(Link(before, after)) = map.get(circle, number)
      assert Ok(Link(before_before, _)) = map.get(circle, before)
      assert Ok(Link(before_before_before, _)) = map.get(circle, before_before)
      assert Ok(Link(_, after_after)) = map.get(circle, after)
      let circle =
        circle
        |> map.insert(after, Link(before, after_after))
        |> map.insert(before, Link(number, after))
        |> map.insert(number, Link(before_before, before))
        |> map.insert(before_before, Link(before_before_before, number))
      swap_left(steps + 1, number, circle)
    }
  }
}

fn swap_right(steps: Int, number: #(Int, Int), circle: Circle) -> Circle {
  case steps {
    0 -> circle
    _ -> {
      // from: before number after after_after 
      // to:   before after number after_after 
      assert Ok(Link(before, after)) = map.get(circle, number)
      assert Ok(Link(before_before, _)) = map.get(circle, before)
      assert Ok(Link(_, after_after)) = map.get(circle, after)
      assert Ok(Link(_, after_after_after)) = map.get(circle, after_after)
      let circle =
        circle
        |> map.insert(after_after, Link(number, after_after_after))
        |> map.insert(number, Link(after, after_after))
        |> map.insert(after, Link(before, number))
        |> map.insert(before, Link(before_before, after))
      swap_right(steps - 1, number, circle)
    }
  }
}

fn find_coordinate(circle: Circle, zero: #(Int, Int)) -> Int {
  let one = jump_right(1000, zero, circle)
  let two = jump_right(1000, one, circle)
  let three = jump_right(1000, two, circle)
  one.1 + two.1 + three.1
}

fn jump_right(steps: Int, current: #(Int, Int), circle: Circle) -> #(Int, Int) {
  case steps {
    0 -> current
    _ -> {
      assert Ok(Link(_, after)) = map.get(circle, current)
      jump_right(steps - 1, after, circle)
    }
  }
}
