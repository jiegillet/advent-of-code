import gleam/erlang/file
import gleam/function
import gleam/list
import gleam/string
import gleam/io
import gleam/result
import gleam/int
import gleam/map.{Map}
import nibble/predicates
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day21.txt")

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
  |> list.map(nibble.run(_, tree_parser()))
  |> result.values
  |> map.from_list
  |> eval("root")
}

pub fn solve_part2(input: String) -> Int {
  let tree =
    input
    |> string.trim
    |> string.split("\n")
    |> list.map(nibble.run(_, tree_parser()))
    |> result.values
    |> map.from_list

  assert Ok(Node(branch1, _, branch2)) = map.get(tree, "root")
  case eval2(tree, branch1) {
    Concrete(value) -> {
      assert Lazy(function) = eval2(tree, branch2)
      find_root(fn(n) { function(n) - value })
    }
    Lazy(function) -> {
      assert Concrete(value) = eval2(tree, branch2)
      find_root(fn(n) { function(n) - value })
    }
  }
}

fn eval(tree: Map(String, Tree), branch: String) -> Int {
  case map.get(tree, branch) {
    Ok(Leaf(value)) -> value
    Ok(Node(branch1, op, branch2)) -> {
      let value1 = eval(tree, branch1)
      let value2 = eval(tree, branch2)
      op(value1, value2)
    }
    _ -> 0
  }
}

type LazyEval {
  Concrete(value: Int)
  Lazy(function: fn(Int) -> Int)
}

fn eval2(tree: Map(String, Tree), branch: String) -> LazyEval {
  case branch, map.get(tree, branch) {
    "humn", _ -> Lazy(function.identity)
    _, Ok(Leaf(value)) -> Concrete(value)
    _, Ok(Node(branch1, op, branch2)) -> {
      let value1 = eval2(tree, branch1)
      let value2 = eval2(tree, branch2)
      combine(op, value1, value2)
    }
    _, _ -> Concrete(0)
  }
}

fn combine(
  op: fn(Int, Int) -> Int,
  value1: LazyEval,
  value2: LazyEval,
) -> LazyEval {
  case value1, value2 {
    Concrete(value1), Concrete(value2) -> Concrete(op(value1, value2))
    Lazy(function), Concrete(value2) -> Lazy(fn(n) { op(function(n), value2) })
    Concrete(value1), Lazy(function) -> Lazy(fn(n) { op(value1, function(n)) })
    _, _ -> Concrete(0)
  }
}

fn find_root(function: fn(Int) -> Int) -> Int {
  let start = -1_000_000_000_000_000_000
  let end = 1_000_000_000_000_000_000
  assert True = function(start) * function(end) < 0
  let answer = bisect(function, start, end)
  // answers are not unique for large numbers because of floor division
  reduce(function, answer)
}

fn bisect(function: fn(Int) -> Int, start: Int, end: Int) -> Int {
  let mid = { start + end } / 2
  case function(mid) * function(start) {
    0 -> mid
    n if n > 0 -> bisect(function, mid, end)
    n if n < 0 -> bisect(function, start, mid)
  }
}

fn reduce(function: fn(Int) -> Int, answer: Int) -> Int {
  case function(answer - 1) == 0 {
    True -> reduce(function, answer - 1)
    False -> answer
  }
}

type Tree {
  Leaf(value: Int)
  Node(branch1: String, op: fn(Int, Int) -> Int, branch2: String)
}

fn tree_parser() -> Parser(#(String, Tree), ctx) {
  nibble.succeed(fn(name) { fn(tree) { #(name, tree) } })
  |> nibble.keep(name_parser())
  |> nibble.drop(nibble.string(": "))
  |> nibble.keep(nibble.one_of([leaf_parser(), node_parser()]))
}

fn leaf_parser() -> Parser(Tree, ctx) {
  nibble.map(nibble.int(), Leaf)
}

fn node_parser() -> Parser(Tree, ctx) {
  nibble.succeed(function.curry3(Node))
  |> nibble.keep(name_parser())
  |> nibble.keep(op_parser())
  |> nibble.keep(name_parser())
}

fn op_parser() -> Parser(fn(Int, Int) -> Int, ctx) {
  nibble.one_of([
    nibble.drop(nibble.succeed(int.add), nibble.string(" + ")),
    nibble.drop(nibble.succeed(int.subtract), nibble.string(" - ")),
    nibble.drop(nibble.succeed(int.multiply), nibble.string(" * ")),
    nibble.drop(nibble.succeed(fn(a, b) { a / b }), nibble.string(" / ")),
  ])
}

fn name_parser() -> Parser(String, ctx) {
  nibble.take_while(predicates.is_alphanum)
}
