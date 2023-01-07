import gleam/erlang/file
import gleam/function
import gleam/list
import gleam/io
import gleam/int
import gleam/map.{Map}
import gleam/option.{Some}
import nibble.{Parser}

pub fn run() {
  assert Ok(input) = file.read("inputs/day11.txt")

  solve_part1(input)
  |> int.to_string
  |> io.println

  solve_part2(input)
  |> int.to_string
  |> io.println
}

pub fn solve_part1(input: String) -> Int {
  solve(input, 20, fn(worry) { worry / 3 })
}

pub fn solve_part2(input: String) -> Int {
  solve(input, 10_000, function.identity)
}

fn solve(input: String, rounds: Int, worry_fn: fn(Worry) -> Worry) {
  assert Ok(keep_away) = nibble.run(input, keep_away_parser())
  let modulo =
    keep_away
    |> map.values
    |> list.map(fn(monkey) { monkey.throw_to.0 })
    |> int.product

  let worry_fn = fn(worry) { worry_fn(worry) % modulo }

  list.range(1, rounds)
  |> list.fold(
    from: keep_away,
    with: fn(keep_away, monkey_num) {
      play_round(worry_fn, keep_away, monkey_num)
    },
  )
  |> monkey_business
}

type Worry =
  Int

type Monkey {
  Monkey(
    items: List(Worry),
    operation: fn(Worry) -> Worry,
    throw_to: #(Int, fn(Worry) -> Int),
    inspections: Int,
  )
}

type KeepAway =
  Map(Int, Monkey)

fn play_round(
  worry_fn: fn(Worry) -> Worry,
  keep_away: KeepAway,
  _round: Int,
) -> KeepAway {
  list.fold(
    over: list.range(0, map.size(keep_away) - 1),
    from: keep_away,
    with: fn(keep_away, monkey_num) {
      inspection(worry_fn, keep_away, monkey_num)
    },
  )
}

fn inspection(
  worry_fn: fn(Worry) -> Worry,
  keep_away: KeepAway,
  monkey_num: Int,
) -> KeepAway {
  assert Ok(monkey) = map.get(keep_away, monkey_num)
  let Monkey(items, operation, throw_to, inspections) = monkey
  let keep_away =
    list.fold(
      over: items,
      from: keep_away,
      with: fn(keep_away, item) {
        let worry = worry_fn(operation(item))
        let monkey_num = throw_to.1(worry)
        map.update(
          keep_away,
          monkey_num,
          fn(monkey) {
            assert Some(monkey) = monkey
            Monkey(..monkey, items: [worry, ..monkey.items])
          },
        )
      },
    )

  let inspections = inspections + list.length(items)
  let monkey = Monkey(..monkey, items: [], inspections: inspections)
  map.insert(keep_away, monkey_num, monkey)
}

fn monkey_business(keep_away: KeepAway) -> Int {
  keep_away
  |> map.values
  |> list.map(fn(monkey) { monkey.inspections })
  |> list.sort(int.compare)
  |> list.reverse
  |> list.take(2)
  |> int.product
}

fn keep_away_parser() -> Parser(KeepAway, ctx) {
  nibble.many(int_monkey_parser(), nibble.string("\n"))
  |> nibble.map(map.from_list)
}

fn int_monkey_parser() -> Parser(#(Int, Monkey), ctx) {
  nibble.succeed(fn(n) { fn(monkey) { #(n, monkey) } })
  |> nibble.drop(nibble.string("Monkey "))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string(":\n"))
  |> nibble.keep(monkey_parser())
}

fn monkey_parser() -> Parser(Monkey, ctx) {
  nibble.succeed(function.curry4(Monkey))
  |> nibble.keep(items_parser())
  |> nibble.keep(operation_parser())
  |> nibble.keep(throws_to_parser())
  |> nibble.keep(nibble.succeed(0))
}

fn items_parser() -> Parser(List(Worry), ctx) {
  nibble.succeed(function.identity)
  |> nibble.drop(nibble.spaces())
  |> nibble.drop(nibble.string("Starting items: "))
  |> nibble.keep(nibble.many(nibble.int(), nibble.string(", ")))
  |> nibble.drop(nibble.string("\n"))
}

type Argument {
  Old
  Custom(Int)
}

fn operation_parser() -> Parser(fn(Worry) -> Worry, ctx) {
  let operation = fn(op, old) {
    fn(worry) {
      case old {
        Old -> op(worry, worry)
        Custom(value) -> op(worry, value)
      }
    }
  }

  nibble.succeed(function.curry2(operation))
  |> nibble.drop(nibble.spaces())
  |> nibble.drop(nibble.string("Operation: new = old "))
  |> nibble.keep(nibble.one_of([
    nibble.drop(nibble.succeed(int.multiply), nibble.string("* ")),
    nibble.drop(nibble.succeed(int.add), nibble.string("+ ")),
  ]))
  |> nibble.keep(nibble.one_of([
    nibble.drop(nibble.succeed(Old), nibble.string("old")),
    nibble.map(nibble.int(), Custom),
  ]))
  |> nibble.drop(nibble.string("\n"))
}

fn throws_to_parser() -> Parser(#(Int, fn(Worry) -> Int), ctx) {
  let throw = fn(divider, a, b) {
    #(
      divider,
      fn(worry) {
        case worry % divider {
          0 -> a
          _ -> b
        }
      },
    )
  }

  nibble.succeed(function.curry3(throw))
  |> nibble.drop(nibble.spaces())
  |> nibble.drop(nibble.string("Test: divisible by "))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string("\n"))
  |> nibble.drop(nibble.spaces())
  |> nibble.drop(nibble.string("If true: throw to monkey "))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string("\n"))
  |> nibble.drop(nibble.spaces())
  |> nibble.drop(nibble.string("If false: throw to monkey "))
  |> nibble.keep(nibble.int())
  |> nibble.drop(nibble.string("\n"))
}
