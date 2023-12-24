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
    input <- File.readUtf8 (Path.fromStr "../inputs/day8.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)

    """

expect solve1 testInput == 6
expect solve2 testInput == 6

solve1 : Str -> Nat
solve1 = \input ->
    when String.parseStr mapP input is
        Err _ -> 0
        Ok (Map directions nodes) ->
            length = List.len directions
            step = \node, count ->
                when (List.get directions (Num.rem count length), Dict.get nodes node) is
                    (Ok L, Ok { left: "ZZZ" }) -> count + 1
                    (Ok R, Ok { right: "ZZZ" }) -> count + 1
                    (Ok L, Ok { left }) -> step left (count + 1)
                    (Ok R, Ok { right }) -> step right (count + 1)
                    _ -> crash "node not found"

            step "AAA" 0

solve2 : Str -> Nat
solve2 = \input ->
    when String.parseStr mapP input is
        Err _ -> 0
        Ok (Map directions nodes) ->
            length = List.len directions

            starts = nodes |> Dict.keys |> List.keepIf (\str -> Str.endsWith str "A")

            # Looks like all starts have cycles that in multiples of length of directions
            findCycle = \node, count ->
                when (List.get directions (Num.rem count length), Dict.get nodes node) is
                    (Ok L, Ok { left }) if Str.endsWith left "Z" -> Num.divTrunc (count + 1) length
                    (Ok R, Ok { right }) if Str.endsWith right "Z" -> Num.divTrunc (count + 1) length
                    (Ok L, Ok { left }) -> findCycle left (count + 1)
                    (Ok R, Ok { right }) -> findCycle right (count + 1)
                    _ -> crash "node not found"

            cycles = List.map starts (\node -> findCycle node 0)
            length * List.walk cycles 1 lcm

lcm = \a, b -> Num.divTrunc (a * b) (gcd a b)

gcd = \a, b ->
    if b == 0 then
        a
    else
        gcd b (Num.rem a b)

# Parser

mapP =
    Core.const (\directions -> \nodes -> Map directions (Dict.fromList nodes))
    |> Core.keep directionsP
    |> Core.skip (String.string "\n\n")
    |> Core.keep (Core.many nodeP)

expect String.parseStr directionsP "LRLRR" == Ok [L, R, L, R, R]

directionsP =
    Core.oneOf [
        Core.skip (Core.const L) (String.codeunit 'L'),
        Core.skip (Core.const R) (String.codeunit 'R'),
    ]
    |> Core.many

expect String.parseStr nodeP "CCC = (ZZZ, GGG)\n" == Ok ("CCC", { left: "ZZZ", right: "GGG" })

nodeP =
    Core.const (\node -> \left -> \right -> (node, { left, right }))
    |> Core.keep nameP
    |> Core.skip (String.string " = (")
    |> Core.keep nameP
    |> Core.skip (String.string ", ")
    |> Core.keep nameP
    |> Core.skip (String.string ")\n")

expect String.parseStr nameP "XYZ" == Ok "XYZ"

nameP =
    Core.chompWhile (\c -> 'A' <= c && c <= 'Z')
    |> Core.map (\chars -> chars |> Str.fromUtf8 |> Result.withDefault "")

