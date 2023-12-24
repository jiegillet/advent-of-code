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
    input <- File.readUtf8 (Path.fromStr "../inputs/day9.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """

expect solve1 testInput == 114
expect solve2 testInput == 2

solve1 = \input ->
    input
    |> Str.split "\n"
    |> List.map (\line -> line |> Str.split " " |> List.keepOks Str.toI128 |> extrapolate)
    |> List.sum

extrapolate = \numbers ->
    diff = \x -> List.map2 (List.dropFirst x 1) x Num.sub
    allZeros = \x -> List.all x (\n -> n == 0)

    do = \x, sum ->
        if allZeros x then
            sum
        else
            do (diff x) (sum + Result.withDefault (List.last x) 0)

    do numbers 0

solve2 = \input ->
    input
    |> Str.split "\n"
    |> List.map (\line -> line |> Str.split " " |> List.keepOks Str.toI128 |> List.reverse |> extrapolate)
    |> List.sum
