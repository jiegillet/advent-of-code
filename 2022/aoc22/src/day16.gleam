import gleam/erlang/file
import gleam/function
import gleam/list
import gleam/string
import gleam/io
import gleam/result
import gleam/option.{None, Some}
import gleam/int
import gleam/map.{Map}
import nibble/predicates
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day16.txt")

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
  |> list.map(nibble.run(_, scan_parser()))
  |> result.values
  |> list.fold(from: Graph(map.new(), map.new()), with: add_node)
  |> fully_connect
  |> fully_connect
  |> release_pressure([Move("AA", 30)])
}

pub fn solve_part2(input: String) -> Int {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(nibble.run(_, scan_parser()))
  |> result.values
  |> list.fold(from: Graph(map.new(), map.new()), with: add_node)
  |> fully_connect
  |> fully_connect
  |> release_pressure_with_elephant([Move("AA", 26), Move("AA", 26)])
}

type Scan {
  Scan(valve: String, flow_rate: Int, tunnels: List(String))
}

type Graph {
  Graph(flow_rates: Map(String, Int), paths: Map(#(String, String), Int))
}

fn add_node(graph: Graph, scan: Scan) -> Graph {
  let Graph(flow_rates, paths) = graph
  let Scan(valve, flow_rate, tunnels) = scan

  let flow_rates = map.insert(flow_rates, valve, flow_rate)
  let paths =
    tunnels
    |> list.map(sort_valves(_, valve))
    |> list.fold(
      from: paths,
      with: fn(paths, key) { map.insert(paths, key, 1) },
    )

  Graph(flow_rates, paths)
}

fn sort_valves(a: String, b: String) -> #(String, String) {
  assert [a, b] = list.sort([a, b], string.compare)
  #(a, b)
}

fn fully_connect(graph: Graph) -> Graph {
  let Graph(flow_rates, paths) = graph
  let valves = map.keys(flow_rates)

  let no_flow_valves =
    valves
    |> list.filter(fn(valve) {
      valve != "AA" && map.get(flow_rates, valve) == Ok(0)
    })

  let flow_rates = map.drop(flow_rates, no_flow_valves)

  let paths =
    paths
    |> map.fold(
      from: paths,
      with: fn(paths, valves, cost) {
        let #(a, b) = valves
        map.fold(
          over: paths,
          from: paths,
          with: fn(paths, tunnel, tunnel_cost) {
            let #(ta, tb) = tunnel
            case Nil {
              _ if ta == a ->
                update_cost(paths, sort_valves(b, tb), tunnel_cost + cost)
              _ if tb == a ->
                update_cost(paths, sort_valves(b, ta), tunnel_cost + cost)
              _ if ta == b ->
                update_cost(paths, sort_valves(a, tb), tunnel_cost + cost)
              _ if tb == b ->
                update_cost(paths, sort_valves(a, ta), tunnel_cost + cost)
              _ -> paths
            }
          },
        )
      },
    )
    |> map.filter(fn(tunnel, _) {
      let #(a, b) = tunnel
      a != b && !list.contains(no_flow_valves, a) && !list.contains(
        no_flow_valves,
        b,
      )
    })

  Graph(flow_rates, paths)
}

fn update_cost(paths: Map(key, Int), tunnel: key, cost: Int) -> Map(key, Int) {
  map.update(
    paths,
    tunnel,
    fn(maybe_b) {
      case maybe_b {
        None -> cost
        Some(c) -> int.min(c, cost)
      }
    },
  )
}

type Move {
  Move(valve: String, remaining: Int)
}

fn release_pressure(graph: Graph, moves: List(Move)) -> Int {
  let Graph(flow_rates, paths) = graph
  assert [Move(previous_step, remaining), ..rest] = moves
  let previous_valves = list.map(moves, fn(move) { move.valve })
  let next_valves =
    flow_rates
    |> map.keys
    |> list.filter(fn(valve) { !list.contains(previous_valves, valve) })
  case next_valves {
    _ if remaining < 0 -> calculate_pressure(graph, rest)
    [] -> calculate_pressure(graph, moves)
    _ ->
      next_valves
      |> list.map(fn(valve) {
        assert Ok(cost) = map.get(paths, sort_valves(previous_step, valve))
        let moves = [Move(valve, remaining - cost - 1), ..moves]
        release_pressure(graph, moves)
      })
      |> list.fold(from: 0, with: int.max)
  }
}

fn release_pressure_with_elephant(graph: Graph, moves: List(Move)) -> Int {
  let Graph(flow_rates, paths) = graph
  assert [Move(prev_step1, remaining1), Move(prev_step2, remaining2), ..rest] =
    moves
  let previous_valves = list.map(moves, fn(move) { move.valve })
  let next_valves =
    flow_rates
    |> map.keys
    |> list.filter(fn(valve) { !list.contains(previous_valves, valve) })
  case next_valves {
    _ if remaining1 < 0 && remaining2 < 0 -> calculate_pressure(graph, rest)
    _ if remaining1 < 0 ->
      release_pressure(graph, [Move(prev_step2, remaining2), ..rest])
    _ if remaining2 < 0 ->
      release_pressure(graph, [Move(prev_step1, remaining1), ..rest])
    [] -> calculate_pressure(graph, moves)
    _ ->
      next_valves
      |> list.combination_pairs
      |> list.flat_map(fn(pair) {
        assert Ok(cost1) = map.get(paths, sort_valves(prev_step1, pair.0))
        assert Ok(cost2) = map.get(paths, sort_valves(prev_step2, pair.1))
        let moves1 = [
          Move(pair.0, remaining1 - cost1 - 1),
          Move(pair.1, remaining2 - cost2 - 1),
          ..moves
        ]
        assert Ok(cost1) = map.get(paths, sort_valves(prev_step1, pair.1))
        assert Ok(cost2) = map.get(paths, sort_valves(prev_step2, pair.0))
        let moves2 = [
          Move(pair.1, remaining1 - cost1 - 1),
          Move(pair.0, remaining2 - cost2 - 1),
          ..moves
        ]
        [
          release_pressure_with_elephant(graph, moves1),
          release_pressure_with_elephant(graph, moves2),
        ]
      })
      |> list.fold(from: 0, with: int.max)
  }
}

fn calculate_pressure(graph: Graph, moves: List(Move)) -> Int {
  let Graph(flow_rates, _) = graph
  moves
  |> list.filter(fn(move) { move.remaining > 0 })
  |> list.map(fn(move) {
    let Move(valve, remaining) = move
    assert Ok(flow) = map.get(flow_rates, valve)
    flow * remaining
  })
  |> int.sum
}

fn scan_parser() -> Parser(Scan, ctx) {
  nibble.succeed(function.curry3(Scan))
  |> nibble.drop(nibble.string("Valve "))
  |> nibble.keep(name_parser())
  |> nibble.drop(nibble.string(" has flow rate="))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.one_of([
    nibble.string("; tunnels lead to valves "),
    nibble.string("; tunnel leads to valve "),
  ]))
  |> nibble.keep(nibble.many(name_parser(), nibble.string(", ")))
}

fn name_parser() -> Parser(String, ctx) {
  nibble.take_while(predicates.is_alphanum)
}
