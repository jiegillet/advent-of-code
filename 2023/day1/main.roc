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
    input <- File.readUtf8 (Path.fromStr "../inputs/day1.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

expect solve1 testInput == 56042
expect solve2 testInput == 281

solve1 : Str -> Nat
solve1 = \input ->
    input
    |> Str.split "\n"
    |> List.map
        (\line ->
            digits = line |> Str.graphemes |> List.keepOks Str.toNat
            when digits is
                [first] -> 10 * first + first
                [first, .., last] -> 10 * first + last
                _ -> 0
        )
    |> List.sum

solve2 : Str -> Nat
solve2 = \input ->
    input
    |> Str.replaceEach "one" "one1one"
    |> Str.replaceEach "two" "two2two"
    |> Str.replaceEach "three" "three3three"
    |> Str.replaceEach "four" "four4four"
    |> Str.replaceEach "five" "five5five"
    |> Str.replaceEach "six" "six6six"
    |> Str.replaceEach "seven" "seven7seven"
    |> Str.replaceEach "eight" "eight8eight"
    |> Str.replaceEach "nine" "nine9nine"
    |> solve1

