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
    input <- File.readUtf8 (Path.fromStr "../inputs/day5.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4

    """

expect solve1 testInput == 35
expect solve2 testInput == 46

solve1 : Str -> Nat
solve1 = \input ->
    transform = \maps, seed ->
        seed2, (Map _ ranges) <- List.walk maps seed
        s, { source, destination, length } <- List.walkUntil ranges seed2
        if source <= s && s <= source + length - 1 then
            Break (s - source + destination)
        else
            Continue s

    when String.parseStr gardenP input is
        Err _ -> 0
        Ok { seeds, maps } ->
            seeds
            |> List.map (\seed -> transform maps seed)
            |> List.min
            |> Result.withDefault 0

solve2 : Str -> Nat
solve2 = \input ->
    expand = \pair ->
        when pair is
            [start, length] ->
                [{ from: start, to: start + length - 1 }]

            _ -> []

    transform = \ranges, maps ->
        classes, (Map _ mapRanges) <- List.walk maps { transformed: [], intact: ranges }
        allIntact = { transformed: [], intact: List.concat classes.intact classes.transformed }
        { transformed, intact }, { source, destination, length } <- List.walk mapRanges allIntact
        result, { from, to } <- List.walk intact { transformed, intact: [] }

        if to < source || from > source + length - 1 then
            { result & intact: List.append result.intact { from, to } }
        else if source <= from && to <= source + length - 1 then
            { result & transformed: List.append result.transformed { from: from - source + destination, to: to - source + destination } }
        else if source <= from && to > source + length - 1 then
            {
                transformed: List.append result.transformed { from: from - source + destination, to: destination + length - 1 },
                intact: List.append result.intact { from: source + length, to },
            }
        else if from < source && to <= source + length - 1 then
            {
                transformed: List.append result.transformed { from: destination, to: to - source + destination },
                intact: List.append result.intact { from, to: source - 1 },
            }
        else
            {
                transformed: List.append result.transformed { from: destination, to: destination + length - 1 },
                intact: List.concat result.intact [{ from, to: source - 1 }, { from: source + length, to }],
            }

    when String.parseStr gardenP input is
        Err _ -> 0
        Ok { seeds, maps } ->
            { transformed, intact } =
                seeds
                |> List.chunksOf 2
                |> List.joinMap expand
                |> transform maps

            List.concat transformed intact
            |> List.map .from
            |> List.min
            |> Result.withDefault 0

# Parser

gardenP =
    Core.const (\seeds -> \maps -> { seeds, maps })
    |> Core.keep seedsP
    |> Core.skip (String.string "\n\n")
    |> Core.keep (Core.sepBy mapP (String.string "\n\n"))
    |> Core.skip (String.string "\n")

expect String.parseStr seedsP "seeds: 79 14 55 13" == Ok ([79, 14, 55, 13])

seedsP =
    Core.const (\id -> id)
    |> Core.skip (String.string "seeds: ")
    |> Core.keep numbersP

expect
    String.parseStr mapP "name map:\n1 2 3\n4 5 6"
    == Ok (Map "name" [{ destination: 1, source: 2, length: 3 }, { destination: 4, source: 5, length: 6 }])

mapP =
    Core.const (\name -> \ranges -> Map name ranges)
    |> Core.apply mapNameP
    |> Core.apply (Core.sepBy rangeP (String.string "\n"))

expect String.parseStr rangeP "1 2 3" == Ok { destination: 1, source: 2, length: 3 }

rangeP =
    Core.const (\destination -> \source -> \length -> { destination, source, length })
    |> Core.keep String.digits
    |> Core.skip (String.string " ")
    |> Core.keep String.digits
    |> Core.skip (String.string " ")
    |> Core.keep String.digits

expect String.parseStr mapNameP "something map:\n" == Ok "something"

mapNameP =
    Core.const (\list -> list |> Str.fromUtf8 |> Result.withDefault "")
    |> Core.keep (Core.chompUntil ' ')
    |> Core.skip (String.string " map:\n")

expect String.parseStr numbersP "1   2 3 3" == Ok [1, 2, 3, 3]

numbersP =
    Core.sepBy String.digits spacesP

expect String.parseStr spacesP "   " == Ok {}

spacesP = Core.skip (Core.const {}) (Core.oneOrMore (String.string " "))

