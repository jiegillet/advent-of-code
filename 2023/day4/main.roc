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
    input <- File.readUtf8 (Path.fromStr "../inputs/day4.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

    """

expect solve1 testInput == 13
expect solve2 testInput == 30

solve1 : Str -> Nat
solve1 = \input ->
    gains = \Card { winning, got } ->
        when Set.len (Set.intersection winning got) is
            0 -> 0
            n -> Num.powInt 2 (n - 1)

    when String.parseStr gameP input is
        Err _ -> 0
        Ok cards ->
            cards
            |> List.map gains
            |> List.sum

solve2 : Str -> Nat
solve2 = \input ->
    accumulate = \{ counts, score }, Card { id, winning, got } ->
        won = Set.len (Set.intersection winning got)
        copiesOfSelf = Result.withDefault (Dict.get counts id) 1

        add = \increase, value -> 
            when value is
                Missing -> Present (1 + increase)
                Present n -> Present (n + increase)

        newCounts =
            List.range { start: After id, end: Length won }
            |> List.walk counts (\c, n -> Dict.update c n (\value -> add (copiesOfSelf) value))

        { score: score + copiesOfSelf, counts: newCounts }

    when String.parseStr gameP input is
        Err _ -> 0
        Ok cards ->
            cards
             |> List.walk ({ counts: Dict.empty {}, score: 0 }) accumulate
             |> .score

# Parser

gameP =
    Core.sepBy cardP (String.string "\n")
    |> Core.skip (String.string "\n")

expect
    String.parseStr cardP "Card  1: 1 2 2 |   4 4 4 3"
    == Ok (Card { id: 1, winning: Set.fromList [1, 2], got: Set.fromList [4, 3] })

expect Set.fromList [1, 2] == Set.fromList [2, 1]

cardP =
    Core.const (\id -> \winning -> \got -> Card { id, winning, got })
    |> Core.keep cardIdP
    |> Core.keep numbersP
    |> Core.skip spacesP
    |> Core.skip (String.string "|")
    |> Core.skip spacesP
    |> Core.keep numbersP

expect String.parseStr numbersP "1   2 3 3" == Ok (Set.fromList [1, 2, 3])

numbersP =
    Core.sepBy String.digits spacesP
    |> Core.map Set.fromList

expect String.parseStr cardIdP "Card   45:   " == Ok 45

cardIdP =
    Core.const (\id -> id)
    |> Core.skip (String.string "Card")
    |> Core.skip spacesP
    |> Core.keep (String.digits)
    |> Core.skip (String.string ":")
    |> Core.skip spacesP

expect String.parseStr spacesP "   " == Ok {}

spacesP = Core.skip (Core.const {}) (Core.oneOrMore (String.string " "))
