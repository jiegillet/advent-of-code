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
    input <- File.readUtf8 (Path.fromStr "../inputs/day2.txt") |> Task.await
    

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """

expect solve1 testInput == 8
expect solve2 testInput == 2286

Color : [Red, Green, Blue]

solve1 : Str -> Nat
solve1 = \input ->
    possible = \color, number ->
        when color is
            Red -> Dict.single (number <= 12) {}
            Green -> Dict.single (number <= 13) {}
            Blue -> Dict.single (number <= 14) {}

    merge = \a, b ->
      Dict.walk a b (\dict, color, number ->  Dict.update dict color (add number))

    add = \number -> \value ->
      when value is
        Present n -> Present (Num.max n number)
        Missing -> Present number

    possibleDrawId = \(id, dicts) ->
      poss =
        dicts
        |> List.walk (Dict.empty {}) merge
        |> Dict.joinMap possible
        |> Dict.contains Bool.false
        |> Bool.not

      if poss then id else 0

    when String.parseStr gameP (Str.trim input) is
        Err err -> 0
        Ok lines ->
            lines
            |> List.map possibleDrawId
            |> List.sum

solve2 : Str -> Nat
solve2 = \input ->
    merge = \a, b ->
      Dict.walk a b (\dict, color, number ->  Dict.update dict color (add number))

    add = \number -> \value ->
      when value is
        Present n -> Present (Num.max n number)
        Missing -> Present number

    possibleDrawId = \(_, dicts) ->
        dicts
        |> List.walk (Dict.empty {}) merge
        |> Dict.values
        |> List.product

    when String.parseStr gameP (Str.trim input) is
        Err err -> 0
        Ok lines ->
            lines
            |> List.map possibleDrawId
            |> List.sum

# Parser

gameP = Core.sepBy lineP (String.string "\n")

expect
    (String.parseStr lineP "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    ==
    Ok (
        1,
        [
            Dict.fromList [(Blue, 3), (Red, 4)],
            Dict.fromList [(Red, 1), (Green, 2), (Blue, 6)],
            Dict.fromList [(Green, 2)],
        ],
    )

lineP =
    Core.const (\id -> \counts -> (id, counts))
    |> Core.keep gameIdP
    |> Core.keep (Core.sepBy drawP (String.string "; "))

expect String.parseStr drawP "3 blue, 4 red" == Ok (Dict.fromList [(Blue, 3), (Red, 4)])

drawP =
    Core.sepBy countP (String.string ", ") |> Core.map Dict.fromList

expect String.parseStr gameIdP "Game 45: " == Ok 45

gameIdP =
    Core.const (\id -> id)
    |> Core.skip (String.string "Game ")
    |> Core.keep (String.digits)
    |> Core.skip (String.string ": ")

expect String.parseStr countP "2 blue" == Ok (Blue, 2)

countP =
    Core.const (\n -> \color -> (color, n))
    |> Core.keep (String.digits)
    |> Core.skip (String.string " ")
    |> Core.keep colorP

expect String.parseStr colorP "blue" == Ok Blue

colorP =
    Core.oneOf [
        String.string "red" |> Core.map (\_ -> Red),
        String.string "green" |> Core.map (\_ -> Green),
        String.string "blue" |> Core.map (\_ -> Blue),
    ]
