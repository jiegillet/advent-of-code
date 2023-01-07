import gleam/erlang/file
import gleam/string
import gleam/list
import gleam/io
import gleam/int
import gleam/result

pub fn run() {
  try input = file.read("inputs/day2.txt")

  solve_part1(input)
  |> io.debug

  solve_part2(input)
  |> io.debug

  Ok(Nil)
}

pub fn solve_part1(input) {
  input
  |> string.split("\n")
  |> list.map(parse_1)
  |> result.values
  |> list.map(score)
  |> int.sum
}

pub fn solve_part2(input) {
  input
  |> string.split("\n")
  |> list.map(parse_2)
  |> result.values
  |> list.map(score)
  |> int.sum
}

fn parse_1(row) {
  case string.split(row, " ") {
    [opponent, you] -> Ok(#(to_janken(opponent), to_janken(you)))
    _ -> Error(Nil)
  }
}

fn parse_2(row) {
  case string.split(row, " ") {
    [opponent, you] -> {
      let opponent = to_janken(opponent)
      let you = find_move(opponent, to_outcome(you))
      Ok(#(opponent, you))
    }
    _ -> Error(Nil)
  }
}

fn score(moves: #(Janken, Janken)) {
  let #(opponent, you) = moves
  move_score(you) + outcome_score(round(opponent, you))
}

type Janken {
  Rock
  Paper
  Scissors
}

type Outcome {
  Win
  Lose
  Tie
}

fn to_janken(string: String) -> Janken {
  case string {
    "A" | "X" -> Rock
    "B" | "Y" -> Paper
    "C" | "Z" -> Scissors
  }
}

fn to_outcome(string: String) -> Outcome {
  case string {
    "X" -> Lose
    "Y" -> Tie
    "Z" -> Win
  }
}

fn move_score(move: Janken) -> Int {
  case move {
    Rock -> 1
    Paper -> 2
    Scissors -> 3
  }
}

fn outcome_score(outcome: Outcome) -> Int {
  case outcome {
    Win -> 6
    Tie -> 3
    Lose -> 0
  }
}

fn round(opponent: Janken, you: Janken) -> Outcome {
  case #(opponent, you) {
    #(Paper, Scissors) -> Win
    #(Scissors, Rock) -> Win
    #(Rock, Paper) -> Win
    #(Scissors, Scissors) -> Tie
    #(Rock, Rock) -> Tie
    #(Paper, Paper) -> Tie
    _ -> Lose
  }
}

fn find_move(opponent: Janken, outcome: Outcome) -> Janken {
  case #(opponent, outcome) {
    #(_, Tie) -> opponent
    #(Paper, Win) -> Scissors
    #(Rock, Win) -> Paper
    #(Scissors, Win) -> Rock
    #(Paper, Lose) -> Rock
    #(Rock, Lose) -> Scissors
    #(Scissors, Lose) -> Paper
  }
}
