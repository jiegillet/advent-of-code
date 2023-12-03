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

emptyDraw = { red: 0, green: 0, blue: 0 }

expect merge { red: 2, green: 0, blue: 3 } { red: 4, green: 0, blue: 1 } == { red: 4, green: 0, blue: 3 }

merge = \a, b ->
    { red: Num.max a.red b.red, green: Num.max a.green b.green, blue: Num.max a.blue b.blue }

solve1 : Str -> Nat
solve1 = \input ->
    possible = \{ red, green, blue } ->
        red <= 12 && green <= 13 && blue <= 14

    possibleDrawId = \(id, draws) ->
        poss =
            draws
            |> List.walk emptyDraw merge
            |> possible

        if poss then id else 0

    when String.parseStr gameP (Str.trim input) is
        Err _ -> 0
        Ok lines ->
            lines
            |> List.map possibleDrawId
            |> List.sum

solve2 : Str -> Nat
solve2 = \input ->
    value = \{ red, green, blue } ->
        red * green * blue

    drawValue = \(_, draws) ->
        draws
        |> List.walk emptyDraw merge
        |> value

    when String.parseStr gameP (Str.trim input) is
        Err _ -> 0
        Ok lines ->
            lines
            |> List.map drawValue
            |> List.sum

# Parser

gameP = Core.sepBy lineP (String.string "\n")

expect
    (String.parseStr lineP "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    ==
    Ok (
        1,
        [
            { red: 4, green: 0, blue: 3 },
            { red: 1, green: 2, blue: 6 },
            { red: 0, green: 2, blue: 0 },
        ],
    )

lineP =
    Core.const (\id -> \counts -> (id, counts))
    |> Core.keep gameIdP
    |> Core.keep (Core.sepBy drawP (String.string "; "))

expect String.parseStr drawP "3 blue, 4 red" == Ok { blue: 3, red: 4, green: 0 }

drawP =
    Core.sepBy countP (String.string ", ")
    |> Core.map (\colors -> List.walk colors emptyDraw (\draw, color -> color draw))

expect String.parseStr gameIdP "Game 45: " == Ok 45

gameIdP =
    Core.const (\id -> id)
    |> Core.skip (String.string "Game ")
    |> Core.keep (String.digits)
    |> Core.skip (String.string ": ")

countP =
    Core.const (\n -> \color -> color n)
    |> Core.keep (String.digits)
    |> Core.skip (String.string " ")
    |> Core.keep colorP

colorP =
    Core.oneOf [
        String.string "red" |> Core.map (\_ -> \value -> \draw -> { draw & red: value }),
        String.string "green" |> Core.map (\_ -> \value -> \draw -> { draw & green: value }),
        String.string "blue" |> Core.map (\_ -> \value -> \draw -> { draw & blue: value }),
    ]
