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
    input <- File.readUtf8 (Path.fromStr "../inputs/day7.txt") |> Task.await

    part1 = solve1 input
    part2 = solve2 input

    _ <- Stdout.line "Solution to part 1: \(Num.toStr part1)" |> Task.await
    Stdout.line "Solution to part 2: \(Num.toStr part2)"

testInput =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483

    """

expect solve1 testInput == 6440
expect solve2 testInput == 5905

solve1 : Str -> Nat
solve1 = \input ->
    when String.parseStr handsP input is
        Err _ -> 0
        Ok hands ->
            hands
            |> List.sortWith compareHands
            |> List.mapWithIndex (\Hand _ bid, rank -> bid * (rank + 1))
            |> List.sum

compareHands = \Hand cardsA _, Hand cardsB _ ->
    compareCards cardsA cardsB

compareCards = \a, b ->
    typeA = getType a
    typeB = getType b
    when compareType typeA typeB is
        LT -> LT
        GT -> GT
        EQ -> List.map2 a b compareCard |> List.findFirst (\ord -> ord != EQ) |> Result.withDefault EQ

getType = \hand ->
    addOne = \value ->
        when value is
            Missing -> Present 1
            Present n -> Present (n + 1)
    countDict = List.walk hand (Dict.empty {}) (\count, card -> Dict.update count card addOne)
    counts = countDict |> Dict.values |> List.sortDesc
    when counts is
        [5] -> Five
        [4, 1] -> Four
        [3, 2] -> FullHouse
        [3, 1, 1] -> Three
        [2, 2, 1] -> TwoPair
        [2, 1, 1, 1] -> OnePair
        _ -> High

compareType = \a, b ->
    indexA = List.findFirstIndex orderedTypes (\type -> type == a) |> Result.withDefault 0
    indexB = List.findFirstIndex orderedTypes (\type -> type == b) |> Result.withDefault 0
    Num.compare indexB indexA

orderedTypes = [Five, Four, FullHouse, Three, TwoPair, OnePair, High]

compareCard = \a, b ->
    indexA = List.findFirstIndex orderedCards (\card -> card == a) |> Result.withDefault 0
    indexB = List.findFirstIndex orderedCards (\card -> card == b) |> Result.withDefault 0
    Num.compare indexB indexA

orderedCards = [A, K, Q, J, T, C 9, C 8, C 7, C 6, C 5, C 4, C 3, C 2]

solve2 : Str -> Nat
solve2 = \input ->
    when String.parseStr handsP input is
        Err _ -> 0
        Ok hands ->
            hands
            |> List.sortWith compareHandsJ
            |> List.mapWithIndex (\Hand _ bid, rank -> bid * (rank + 1))
            |> List.sum

compareHandsJ = \Hand cardsA _, Hand cardsB _ ->
    compareCardsJ cardsA cardsB

compareCardsJ = \a, b ->
    typeA = getTypeJ a
    typeB = getTypeJ b
    when compareType typeA typeB is
        LT -> LT
        GT -> GT
        EQ -> List.map2 a b compareCardJ |> List.findFirst (\ord -> ord != EQ) |> Result.withDefault EQ

getTypeJ = \hand ->
    addOne = \value ->
        when value is
            Missing -> Present 1
            Present n -> Present (n + 1)
    countDict = List.walk hand (Dict.empty {}) (\count, card -> Dict.update count card addOne)
    counts = countDict |> Dict.remove J |> Dict.values |> List.sortDesc
    when counts is
        # no J
        [5] -> Five
        [4, 1] -> Four
        [3, 2] -> FullHouse
        [3, 1, 1] -> Three
        [2, 2, 1] -> TwoPair
        [2, 1, 1, 1] -> OnePair
        [1, 1, 1, 1, 1] -> High
        # one J
        [4] -> Five
        [3, 1] -> Four
        [2, 2] -> FullHouse
        [2, 1, 1] -> Three
        [1, 1, 1, 1] -> OnePair
        # two J
        [3] -> Five
        [2, 1] -> Four
        [1, 1, 1] -> Three
        # three J
        [2] -> Five
        [1, 1] -> Four
        # four J
        _ -> Five

compareCardJ = \a, b ->
    indexA = List.findFirstIndex orderedCardsJ (\card -> card == a) |> Result.withDefault 0
    indexB = List.findFirstIndex orderedCardsJ (\card -> card == b) |> Result.withDefault 0
    Num.compare indexB indexA

orderedCardsJ = [A, K, Q, T, C 9, C 8, C 7, C 6, C 5, C 4, C 3, C 2, J]

# Parser

handsP = Core.many handP

expect String.parseStr handP "T55J5 684\n" == Ok (Hand [T, C 5, C 5, J, C 5] 684)

handP =
    Core.const (\cards -> \rank -> Hand cards rank)
    |> Core.keep (Core.many cardP)
    |> Core.skip spacesP
    |> Core.keep String.digits
    |> Core.skip (String.string "\n")

expect String.parseStr (Core.many cardP) "AKQJT98765432" == Ok orderedCards

cardP =
    List.map2
        (Str.toUtf8 "AKQJT98765432")
        orderedCards
        (\char, card -> Core.skip (Core.const card) (String.codeunit char))
    |> Core.oneOf

expect String.parseStr spacesP "   " == Ok {}

spacesP = Core.skip (Core.const {}) (Core.oneOrMore (String.string " "))

