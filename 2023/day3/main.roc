app "aoc"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.File, pf.Task, pf.Path]
    provides [main] to pf

main =
    result <- Task.attempt solve
    when result is
        Ok {} -> Task.ok {}
        Err _ -> Stderr.line "IO error" |> Task.await (\_ -> Task.err 1)

solve =
    input <- File.readUtf8 (Path.fromStr "../inputs/day3.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

expect solve1 testInput == 4361
expect solve2 testInput == 467835

toMap = \input ->
    input
    |> Str.graphemes
    |> List.walk
        { symbols: [], row: 0, col: 0, numbers: [], num: None }
        (\state, char ->
            { symbols, row, col, numbers, num } = state
            newState =
                when (num, Str.toNat char) is
                    (None, Err _) -> { state & num: None }
                    (None, Ok digit) ->
                        { state & num: Number { value: digit, row: row, from: col, to: col } }

                    (Number number, Ok digit) ->
                        { state & num: Number { number & value: (10 * number.value + digit), to: number.to + 1 } }

                    (Number number, Err _) -> { state & num: None, numbers: List.append numbers number }

            when (char, Str.toNat char) is
                ("\n", _) -> { newState & col: 0, row: row + 1 }
                (".", _) | (_, Ok _) -> { newState & col: col + 1 }
                (symbol, Err _) -> { newState & col: col + 1, symbols: List.append symbols (Symbol (row, col) symbol) }
                _ -> crash "shouldn't happen"
        )

isAdjacent = \symbols, { row, from, to } ->
    List.any symbols (\Symbol (r, c) _ -> row - 1 <= r && r <= row + 1 && from - 1 <= c && c <= to + 1)

solve1 : Str -> Nat
solve1 = \input ->
    { symbols, numbers } = toMap input
    numbers
    |> List.keepIf (\number -> isAdjacent symbols number)
    |> List.map (\{ value } -> value)
    |> List.sum

gearRatio = \(r, c), numbers ->
    nums =
        List.keepIf numbers (\{ row, from, to } -> row - 1 <= r && r <= row + 1 && from - 1 <= c && c <= to + 1)
    when nums is
        [a, b] -> a.value * b.value
        _ -> 0

solve2 : Str -> Nat
solve2 = \input ->
    { symbols, numbers } = toMap input
    symbols
    |> List.keepIf (\Symbol _ s -> s == "*")
    |> List.map (\Symbol position _ -> gearRatio position numbers)
    |> List.sum

