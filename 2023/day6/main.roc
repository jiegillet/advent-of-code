app "aoc"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Task,
        pf.Path,
        parser.Core,
        parser.String,
    ]
    provides [main] to pf

main =
    result <- Task.attempt solve
    when result is
        Ok {} -> Task.ok {}
        Err _ -> Stderr.line "IO error" |> Task.await (\_ -> Task.err 1)

solve =
    input <- File.readUtf8 (Path.fromStr "../inputs/day6.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    Time:      7  15   30
    Distance:  9  40  200

    """

expect solve1 testInput == 288
expect solve2 testInput == 71503

solve1 : Str -> Nat
solve1 = \input ->
    when String.parseStr racesP input is
        Err _ -> 0
        Ok races ->
            races
            |> List.map waysToWin
            |> List.product

solve2 : Str -> Nat
solve2 = \input ->
    toRace = \R t d -> Race (t |> Str.toNat |> Result.withDefault 0) (d |> Str.toNat |> Result.withDefault 0)

    when String.parseStr racesP input is
        Err _ -> 0
        Ok races ->
            races
            |> List.walk (R "" "") (\R a b, Race t d -> R (Str.concat a (Num.toStr t)) (Str.concat b (Num.toStr d)))
            |> toRace
            |> waysToWin

waysToWin = \Race timeN distN ->
    # (time - t) * t > dist
    # - t*t + time * t - dist > 0
    # t0/1 = (time +/- sqrt( time*time - 4 * dist )) / 2

    time = Num.toF64 timeN
    dist = Num.toF64 distN
    radicand = Num.sqrt (time * time - 4 * dist)
    left = Num.ceiling ((time - radicand) / 2)
    right = Num.floor ((time + radicand) / 2)

    correctionLeft = if (timeN - left) * left == distN then 1 else 0
    correctionRight = if (timeN - right) * right == distN then 1 else 0

    right - left + 1 - correctionLeft - correctionRight

# Parser

racesP =
    Core.const (\time -> \dist -> List.map2 time dist Race)
    |> Core.keep timeP
    |> Core.skip (String.string "\n")
    |> Core.keep distanceP
    |> Core.skip (String.string "\n")

expect String.parseStr timeP "Time:      7  15   30" == Ok ([7, 15, 30])

timeP =
    Core.const (\id -> id)
    |> Core.skip (String.string "Time:")
    |> Core.skip spacesP
    |> Core.keep numbersP

expect String.parseStr distanceP "Distance:      7  15   30" == Ok ([7, 15, 30])

distanceP =
    Core.const (\id -> id)
    |> Core.skip (String.string "Distance:")
    |> Core.skip spacesP
    |> Core.keep numbersP

expect String.parseStr numbersP "1   2 3 3" == Ok [1, 2, 3, 3]

numbersP =
    Core.sepBy String.digits spacesP

expect String.parseStr spacesP "   " == Ok {}

spacesP = Core.skip (Core.const {}) (Core.oneOrMore (String.string " "))

