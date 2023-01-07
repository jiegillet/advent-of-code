import gleam/erlang/file
import gleam/function
import gleam/list
import gleam/io
import gleam/int
import nibble/predicates
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day7.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  assert Ok(tree) = nibble.run(input, tree_parser("/"))

  tree
  |> directory_sizes
  |> list.filter(fn(s) { s <= 100_000 })
  |> int.sum
}

pub fn solve_part2(input: String) -> Int {
  assert Ok(tree) = nibble.run(input, tree_parser("/"))

  assert Directory(size: total_used, ..) = tree
  let total_size = 70_000_000
  let need_free = 30_000_000
  let to_free = need_free - { total_size - total_used }

  tree
  |> directory_sizes
  |> list.filter(fn(s) { s >= to_free })
  |> list.fold(from: total_used, with: int.min)
}

type DirTree {
  File(size: Int, name: String)
  Directory(name: String, size: Int, branches: List(DirTree))
}

fn get_size(branch: DirTree) -> Int {
  case branch {
    File(size: size, ..) -> size
    Directory(size: size, ..) -> size
  }
}

fn directory_sizes(tree: DirTree) -> List(Int) {
  case tree {
    File(..) -> []
    Directory(size: size, branches: branches, ..) -> {
      let branch_sizes = list.flat_map(branches, directory_sizes)
      [size, ..branch_sizes]
    }
  }
}

fn tree_parser(root: String) -> Parser(DirTree, ctx) {
  nibble.succeed(function.identity)
  |> nibble.drop(nibble.string("$ cd " <> root <> "\n"))
  |> nibble.drop(nibble.string("$ ls\n"))
  |> nibble.keep(nibble.many(content_parser(), nibble.succeed(Nil)))
  |> nibble.then(fn(branches) {
    branches
    |> list.map(fn(branch) {
      case branch {
        Directory(name: name, ..) ->
          tree_parser(name)
          |> nibble.drop(backtrack_parser())

        file -> nibble.succeed(file)
      }
    })
    |> traverse
    |> nibble.map(to_directory(root, _))
  })
}

fn to_directory(root: String, branches: List(DirTree)) -> DirTree {
  let size =
    branches
    |> list.map(get_size)
    |> int.sum

  Directory(root, size, branches)
}

fn traverse(parsers: List(Parser(a, ctx))) -> Parser(List(a), ctx) {
  list.fold(
    over: parsers,
    from: nibble.succeed([]),
    with: fn(parsers, parser) {
      nibble.succeed(function.curry2(list.prepend))
      |> nibble.keep(parsers)
      |> nibble.keep(parser)
    },
  )
}

fn content_parser() -> Parser(DirTree, ctx) {
  nibble.one_of([directory_parser(), file_parser()])
}

fn directory_parser() -> Parser(DirTree, ctx) {
  nibble.succeed(function.curry3(Directory))
  |> nibble.drop(nibble.string("dir "))
  |> nibble.keep(name_parser())
  |> nibble.drop(nibble.string("\n"))
  |> nibble.keep(nibble.succeed(0))
  |> nibble.keep(nibble.succeed([]))
}

fn file_parser() -> Parser(DirTree, ctx) {
  nibble.succeed(function.curry2(File))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.spaces())
  |> nibble.keep(name_parser())
  |> nibble.drop(nibble.string("\n"))
}

fn name_parser() -> Parser(String, ctx) {
  nibble.take_until(predicates.is_whitespace)
}

fn backtrack_parser() -> Parser(Nil, ctx) {
  nibble.one_of([nibble.string("$ cd ..\n"), nibble.succeed(Nil)])
}
