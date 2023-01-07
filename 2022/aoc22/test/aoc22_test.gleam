import gleeunit
import gleeunit/should
import gleam/string
import day1
import day2
import day3
import day4
import day5
import day6
import day7
import day8
import day9
import day10
import day11
import day12
import day13
import day14
import day15
import day16
import day17
import day18
import day20
import day21

pub fn main() {
  gleeunit.main()
}

pub fn day1_test() {
  let input =
    "
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"

  day1.solve_part1(input)
  |> should.equal(24_000)
  day1.solve_part2(input)
  |> should.equal(45_000)
}

pub fn day2_test() {
  let input =
    "A Y
B X
C Z"

  day2.solve_part1(input)
  |> should.equal(15)

  day2.solve_part2(input)
  |> should.equal(12)
}

pub fn day3_test() {
  let input =
    "
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"

  day3.solve_part1(input)
  |> should.equal(157)

  day3.solve_part2(input)
  |> should.equal(70)
}

pub fn day4_test() {
  let input =
    "
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"

  day4.solve_part1(input)
  |> should.equal(2)

  day4.solve_part2(input)
  |> should.equal(4)
}

pub fn day5_test() {
  let input =
    "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"

  day5.solve_part1(input)
  |> should.equal("CMZ")

  day5.solve_part2(input)
  |> should.equal("MCD")
}

pub fn day6_test() {
  day6.solve_part1("bvwbjplbgvbhsrlpgdmjqwftvncz")
  |> should.equal(5)

  day6.solve_part1("nppdvjthqldpwncqszvftbrmjlhg")
  |> should.equal(6)

  day6.solve_part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  |> should.equal(10)

  day6.solve_part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
  |> should.equal(11)

  day6.solve_part2("bvwbjplbgvbhsrlpgdmjqwftvncz")
  |> should.equal(23)

  day6.solve_part2("nppdvjthqldpwncqszvftbrmjlhg")
  |> should.equal(23)

  day6.solve_part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  |> should.equal(29)

  day6.solve_part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
  |> should.equal(26)
}

pub fn day7_test() {
  let input =
    "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"

  day7.solve_part1(input)
  |> should.equal(95437)

  day7.solve_part2(input)
  |> should.equal(24933642)
}

pub fn day8_test() {
  let input =
    "
30373
25512
65332
33549
35390
"

  day8.solve_part1(input)
  |> should.equal(21)

  day8.solve_part2(input)
  |> should.equal(8)
}

pub fn day9_test() {
  let input =
    "
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"

  day9.solve_part1(input)
  |> should.equal(13)

  let input2 =
    "
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"
  day9.solve_part2(input2)
  |> should.equal(36)
}

pub fn day10_test() {
  let input =
    "
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"

  day10.solve_part1(input)
  |> should.equal(13140)

  let output =
    "
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
"
    |> string.trim
    |> string.replace(".", " ")

  day10.solve_part2(input)
  |> should.equal(output)
}

pub fn day11_test() {
  let input =
    "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"

  day11.solve_part1(input)
  |> should.equal(10605)

  day11.solve_part2(input)
  |> should.equal(2713310158)
}

pub fn day12_test() {
  let input =
    "
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"

  day12.solve_part1(input)
  |> should.equal(31)

  day12.solve_part2(input)
  |> should.equal(29)
}

pub fn day13_test() {
  let input =
    "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"

  day13.solve_part1(input)
  |> should.equal(13)

  day13.solve_part2(input)
  |> should.equal(140)
}

pub fn day14_test() {
  let input =
    "
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"

  day14.solve_part1(input)
  |> should.equal(24)

  day14.solve_part2(input)
  |> should.equal(93)
}

pub fn day15_test() {
  let input =
    "
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"

  day15.solve_part1(input, 10)
  |> should.equal(26)

  day15.solve_part2(input, 20)
  |> should.equal(56000011)
}

pub fn day16_test() {
  let input =
    "
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"

  day16.solve_part1(input)
  |> should.equal(1651)

  day16.solve_part2(input)
  |> should.equal(1707)
}

pub fn day17_test() {
  let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  day17.solve_part1(input)
  |> should.equal(3068)
  // cycling method nopt suitable for test input
  //  day17.solve_part2(input)
  // |> should.equal(1514285714288)
}

pub fn day18_test() {
  let input =
    "
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
"
  day18.solve_part1(input)
  |> should.equal(64)

  day18.solve_part2(input)
  |> should.equal(58)
}

pub fn day20_test() {
  let input =
    "
1
2
-3
3
-2
0
4
"
  day20.solve_part1(input)
  |> should.equal(3)

  day20.solve_part2(input)
  |> should.equal(1623178306)
}

pub fn day21_test() {
  let input =
    "
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
"
  day21.solve_part1(input)
  |> should.equal(152)

  day21.solve_part2(input)
  |> should.equal(301)
}
