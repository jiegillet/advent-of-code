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
    input <- File.readUtf8 (Path.fromStr "../inputs/day10.txt") |> Task.await

    part1 = solve1 (Str.toUtf8 input)
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    Str.toUtf8
        """
        7-F7-
        .FJS7
        FJLL7
        |F--J
        LJ.LJ
        """

testInput2 =
    """
    FF7FSF7F7F7F7F7F---7
    L|LJ||||||||||||F--J
    FL-7LJLJ||||||LJL-77
    F--JF--7||LJLJ7F7FJ-
    L---JF-JLJ.||-FJLJJ7
    |F|F-JF---7F7-L7L|7|
    |FFJF7L7F-JF7|JL---7
    7-L-JL7||F7|L7F-7F7|
    L.L7LFJ|||||FJL7||LJ
    L7JLJL-JLJLJL--JLJ.L
    """
testInput3 =
    """
    ...........
    .S-------7.
    .|F-----7|.
    .||.....||.
    .||.....||.
    .|L-7.F-J|.
    .|..|.|..|.
    .L--J.L--J.
    ...........
    """

expect solve1 testInput == 8
expect solve2 testInput2 == 10
expect solve2 testInput3 == 4

solve1 : List U8 -> Nat
solve1 = \map ->
    width = 1 + (Result.withDefault (List.findFirstIndex map (\c -> c == '\n')) 0)
    start = List.findFirstIndex map (\c -> c == 'S') |> Result.withDefault 0 |> toCoord width

    up = { row: start.row - 1, col: start.col, steps: 1, from: Down }
    down = { row: start.row + 1, col: start.col, steps: 1, from: Up }
    left = { row: start.row, col: start.col - 1, steps: 1, from: Right }
    right = { row: start.row, col: start.col + 1, steps: 1, from: Left }
    firstStep =
        if List.contains [Ok '|', Ok 'F', Ok '7'] (List.get map (fromCoord up width)) then
            up
        else if List.contains [Ok '|', Ok 'J', Ok 'L'] (List.get map (fromCoord down width)) then
            down
        else if List.contains [Ok '-', Ok 'F', Ok 'L'] (List.get map (fromCoord left width)) then
            left
        else
            right

    go = \{ row, col, from, steps } ->
        when (from, List.get map (fromCoord { row, col } width)) is
            (_, Ok 'S') -> Num.divTrunc steps 2
            (Down, Ok 'F') -> go { steps: steps + 1, row, col: col + 1, from: Left }
            (Right, Ok 'F') -> go { steps: steps + 1, row: row + 1, col, from: Up }
            (Down, Ok '7') -> go { steps: steps + 1, row, col: col - 1, from: Right }
            (Left, Ok '7') -> go { steps: steps + 1, row: row + 1, col, from: Up }
            (Up, Ok 'J') -> go { steps: steps + 1, row, col: col - 1, from: Right }
            (Left, Ok 'J') -> go { steps: steps + 1, row: row - 1, col, from: Down }
            (Up, Ok 'L') -> go { steps: steps + 1, row, col: col + 1, from: Left }
            (Right, Ok 'L') -> go { steps: steps + 1, row: row - 1, col, from: Down }
            (Left, Ok '-') -> go { steps: steps + 1, row, col: col + 1, from: Left }
            (Right, Ok '-') -> go { steps: steps + 1, row, col: col - 1, from: Right }
            (Up, Ok '|') -> go { steps: steps + 1, row: row + 1, col, from: Up }
            (Down, Ok '|') -> go { steps: steps + 1, row: row - 1, col, from: Down }
            _ -> crash "impossible to come from that direction"

    go firstStep

toCoord = \index, width ->
    { row: Num.divTrunc index width, col: Num.rem index width }

fromCoord = \{ row, col }, width ->
    row * width + col

solve2 = \input ->
    map =
        input
        |> Str.split "\n"
        |> List.mapWithIndex
            (\line, row ->
                line
                |> Str.graphemes
                |> List.mapWithIndex (\char, col -> ({ row: Num.toI32 row, col: Num.toI32 col }, char))
            )
        |> List.join
        |> Dict.fromList

    start = Dict.walkUntil map { row: 0, col: 0 } (\zero, pos, char -> if char == "S" then Break pos else Continue zero)


    blank = { row: 0, col: 0, from: Up, path: Set.withCapacity (Dict.len map), left:  Set.withCapacity (Dict.len map), right:  Set.withCapacity (Dict.len map) }
    firstStep =
        if List.contains [Ok "|", Ok "F", Ok "7"] (Dict.get map ({ row: start.row - 1, col: start.col })) then
            { blank & row: start.row - 1, col: start.col, from: Down }
        else if List.contains [Ok "|", Ok "J", Ok "L"] (Dict.get map ({ row: start.row + 1, col: start.col })) then
            { blank & row: start.row + 1, col: start.col, from: Up }
        else
            { blank & row: start.row, col: start.col + 1, from: Left }

    dbg
        firstStep


    go = \state ->
        { row, col, from } = state
        path = Set.insert state.path { row, col }
        
        # 345
        # 2 6
        # 187 

        (leftPos, rightPos) = when (from, Dict.get map ({ row, col })) is
            (_, Ok "S") -> ([], [])
            (Down, Ok "F") -> ([1, 2, 3,4, 5 ], [7]) 
            (Down, Ok "7") -> ([1], [3, 4, 5, 6, 7])
            (Down, Ok "|") -> ([1, 2, 3], [5, 6, 7])
            (Up, Ok "J") ->   ([1, 5, 6, 7, 8], [3])
            (Up, Ok "L") ->   ([5], [1, 2, 3, 7, 8])
            (Up, Ok "|") ->   ([5, 6, 7], [1, 2, 3])
            (Right, Ok "F") ->([7], [1, 2, 3, 4, 5])
            (Right, Ok "L") ->([1, 2, 3, 7, 8], [5])
            (Right, Ok "-") ->([1, 7, 8], [3, 4, 5])
            (Left, Ok "7") -> ([3, 4, 5, 6, 7], [1])
            (Left, Ok "J") -> ([3], [1, 5, 6, 7, 8])
            (Left, Ok "-") -> ([3, 4, 5], [1, 8, 7])
            _ -> crash "impossible to come from that direction"

        codeToCoord = \code ->
          when code is
           1 -> {row: row + 1, col: col - 1}
           2 -> {row, col: col - 1}
           3 -> {row: row - 1, col: col - 1}
           4 -> {row: row - 1, col }
           5 -> {row: row - 1, col: col + 1}
           6 -> {row, col: col + 1}
           7 -> {row: row + 1, col: col + 1}
           _ -> {row: row + 1, col}

        left = Set.union state.left (leftPos |> List.map codeToCoord |> Set.fromList )
        right = Set.union state.right (rightPos |> List.map codeToCoord |> Set.fromList )

        newState = { state & path, left, right }

        when (from, Dict.get map ({ row, col })) is
            (_, Ok "S") -> { path, left: Set.difference left path, right: Set.difference right path }
            (Down, Ok "F") ->  go { newState & row, col: col + 1, from: Left }
            (Right, Ok "F") -> go { newState & row: row + 1, col, from: Up }
            (Down, Ok "7") ->  go { newState & row, col: col - 1, from: Right }
            (Left, Ok "7") ->  go { newState & row: row + 1, col, from: Up }
            (Up, Ok "J") ->    go { newState & row, col: col - 1, from: Right }
            (Left, Ok "J") ->  go { newState & row: row - 1, col, from: Down }
            (Up, Ok "L") ->    go { newState & row, col: col + 1, from: Left }
            (Right, Ok "L") -> go { newState & row: row - 1, col, from: Down }
            (Left, Ok "-") ->  go { newState & row, col: col + 1, from: Left }
            (Right, Ok "-") -> go { newState & row, col: col - 1, from: Right }
            (Up, Ok "|") ->    go { newState & row: row + 1, col, from: Up }
            (Down, Ok "|") ->  go { newState & row: row - 1, col, from: Down }
            _ -> crash "impossible to come from that direction"

    result = go firstStep

    maxLeft = result.left |> Set.toList |> List.map .col |> List.max |> Result.withDefault 0
    maxRight = result.right |> Set.toList |> List.map .col |> List.max |> Result.withDefault 0
    dbg M maxLeft  maxRight
    dbg Set.intersection result.left result.right
    insideSet = if maxLeft > maxRight then result.right else result.left
    dbg insideSet
    fill result.path (Set.empty {}) (Set.toList insideSet)

fill = \path, visited, inside ->
    # this should be a set all the time, but it crashes with 'Integer multiplication overflowed!'
    insideSet = Set.fromList inside
    dbg
        List.len inside

    if List.len inside == 0 then
        Set.len visited
    else
        expanded =
            insideSet
            |> Set.joinMap
                (\{ row, col } ->
                    Set.fromList [{ row, col: col + 1 }, { row, col: col - 1 }, { col, row: row + 1 }, { col, row: row - 1 }])
            |> Set.difference path
            |> Set.difference insideSet
            |> Set.difference visited
            |> Set.toList

        fill path (Set.union visited insideSet) expanded

