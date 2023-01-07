import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/set.{Set}
import gleam/io
import gleam/result
import gleam/int

pub fn run() {
  assert Ok(input) = file.read("inputs/day18.txt")

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
  |> list.map(to_voxel)
  |> surface_area
}

pub fn solve_part2(input: String) -> Int {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(to_voxel)
  |> exterior_surface_area
}

type Voxel {
  Voxel(x: Int, y: Int, z: Int)
}

fn to_voxel(input: String) -> Voxel {
  assert [x, y, z] =
    input
    |> string.split(",")
    |> list.map(int.parse)
    |> result.values

  Voxel(x, y, z)
}

fn surface_area(voxels: List(Voxel)) -> Int {
  let voxels = set.from_list(voxels)

  voxels
  |> set.to_list
  |> list.map(exposed_sides(_, voxels))
  |> int.sum
}

fn exposed_sides(voxel: Voxel, voxels: Set(Voxel)) -> Int {
  voxel
  |> neighbors
  |> list.filter(fn(vox) { !set.contains(voxels, vox) })
  |> list.length
}

fn neighbors(voxel: Voxel) -> List(Voxel) {
  let Voxel(x, y, z) = voxel
  [
    Voxel(x + 1, y, z),
    Voxel(x - 1, y, z),
    Voxel(x, y + 1, z),
    Voxel(x, y - 1, z),
    Voxel(x, y, z + 1),
    Voxel(x, y, z - 1),
  ]
}

fn exterior_surface_area(voxels: List(Voxel)) -> Int {
  assert Ok(Voxel(min_x, min_y, min_z)) =
    list.reduce(
      voxels,
      fn(a, b) {
        Voxel(int.min(a.x, b.x), int.min(a.y, b.y), int.min(a.z, b.z))
      },
    )
  assert Ok(Voxel(max_x, max_y, max_z)) =
    list.reduce(
      voxels,
      fn(a, b) {
        Voxel(int.max(a.x, b.x), int.max(a.y, b.y), int.max(a.z, b.z))
      },
    )
  let box = #(
    Voxel(min_x - 1, min_y - 1, min_z - 1),
    Voxel(max_x + 1, max_y + 1, max_z + 1),
  )
  let ball = set.from_list(voxels)

  expand([box.0], box, ball, set.from_list([box.0]))
  |> set.to_list
  |> list.map(fn(vox) { 6 - exposed_sides(vox, ball) })
  |> int.sum
}

fn expand(
  voxels: List(Voxel),
  box: #(Voxel, Voxel),
  ball: Set(Voxel),
  visited: Set(Voxel),
) -> Set(Voxel) {
  case voxels {
    [] -> visited
    [voxel, ..rest] -> {
      let next =
        voxel
        |> neighbors
        |> list.filter(inside_box(_, box))
        |> list.filter(fn(vox) { !set.contains(visited, vox) })
        |> list.filter(fn(vox) { !set.contains(ball, vox) })
      let visited = list.fold(over: next, from: visited, with: set.insert)
      expand(list.append(next, rest), box, ball, visited)
    }
  }
}

fn inside_box(voxel: Voxel, box: #(Voxel, Voxel)) -> Bool {
  let #(Voxel(min_x, min_y, min_z), Voxel(max_x, max_y, max_z)) = box
  let Voxel(x, y, z) = voxel
  min_x <= x && x <= max_x && min_y <= y && y <= max_y && min_z <= z && z <= max_z
}
