import gleam/erlang/file
import gleam/string
import gleam/list.{Continue, Stop}
import gleam/map.{Map}
import gleam/io
import gleam/int
import gleam/result

pub fn run() {
  assert Ok(input) = file.read("inputs/day8.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  input
  |> parse_forest
  |> count_visible_trees
}

pub fn solve_part2(input: String) -> Int {
  input
  |> parse_forest
  |> max_scenic_score
}

type Forest {
  Forest(width: Int, height: Int, trees: Map(#(Int, Int), #(Int, Bool)))
}

fn parse_forest(input: String) -> Forest {
  let rows =
    input
    |> string.trim
    |> string.split("\n")

  let height = list.length(rows)
  assert Ok(width) =
    rows
    |> list.first
    |> result.map(string.length)

  let trees =
    list.index_map(
      rows,
      fn(i, row) {
        row
        |> string.to_graphemes
        |> list.map(int.parse)
        |> result.values
        |> list.index_map(fn(j, tree) { #(#(i, j), #(tree, False)) })
      },
    )
    |> list.flatten
    |> map.from_list

  Forest(width, height, trees)
}

fn count_visible_trees(forest: Forest) -> Int {
  let Forest(width, height, _) = forest
  let down = list.range(0, height - 1)
  let up = list.range(height - 1, 0)
  let right = list.range(0, width - 1)
  let left = list.range(width - 1, 0)

  let paths =
    list.flatten([
      list.map(down, fn(i) { list.map(right, fn(j) { #(i, j) }) }),
      list.map(down, fn(i) { list.map(left, fn(j) { #(i, j) }) }),
      list.map(right, fn(j) { list.map(up, fn(i) { #(i, j) }) }),
      list.map(right, fn(j) { list.map(down, fn(i) { #(i, j) }) }),
    ])

  let Forest(trees: trees, ..) =
    list.fold(over: paths, from: forest, with: visible_in_row)

  trees
  |> map.filter(fn(_, tree) { tree.1 })
  |> map.size
}

fn visible_in_row(forest: Forest, row: List(#(Int, Int))) -> Forest {
  let Forest(trees: trees, ..) = forest

  let #(_, visible_trees) =
    list.fold(
      over: row,
      from: #(-1, trees),
      with: fn(acc, position) {
        let #(current_max, trees) = acc
        case map.get(trees, position) {
          Ok(#(height, _)) if height > current_max -> #(
            height,
            map.insert(trees, position, #(height, True)),
          )
          _ -> acc
        }
      },
    )

  Forest(..forest, trees: visible_trees)
}

fn max_scenic_score(forest: Forest) -> Int {
  let Forest(width, height, trees) = forest
  map.fold(
    over: trees,
    from: 0,
    with: fn(max_score, position, tree) {
      let #(tree_height, _) = tree
      let #(row, col) = position

      let up = list.map(list.range(row, 0), fn(i) { #(i, col) })
      let down = list.map(list.range(row, height - 1), fn(i) { #(i, col) })
      let left = list.map(list.range(col, 0), fn(j) { #(row, j) })
      let right = list.map(list.range(col, width - 1), fn(j) { #(row, j) })

      let score =
        [up, down, left, right]
        |> list.map(viewing_distance(forest, tree_height, _))
        |> int.product

      int.max(max_score, score)
    },
  )
}

fn viewing_distance(
  forest: Forest,
  max_height: Int,
  row: List(#(Int, Int)),
) -> Int {
  let Forest(trees: trees, ..) = forest
  row
  |> list.drop(1)
  |> list.fold_until(
    from: 0,
    with: fn(view, position) {
      case map.get(trees, position) {
        Ok(#(height, _)) if height < max_height -> Continue(view + 1)
        _ -> Stop(view + 1)
      }
    },
  )
}
