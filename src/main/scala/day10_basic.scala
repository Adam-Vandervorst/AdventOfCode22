package aoc22.day10_basic

import aoc22.summarizeResource


enum Instruction(val cycles: Int):
  case AddX(x: Int) extends Instruction(2)
  case Noop extends Instruction(1)

val addx_pattern = raw"addx (-?\d+)".r
def parse_line(line: String): Instruction = line match
  case "noop" => Instruction.Noop
  case addx_pattern(cycles) => Instruction.AddX(cycles.toInt)
  case _ => throw RuntimeException(s"Invalid line: $line")

def execute(instructions: Iterator[Instruction]): Iterator[Int] =
  var value = 1

  for instruction <- instructions
      i <- 1 to instruction.cycles yield
    if i < instruction.cycles then value
    else instruction match
      case Instruction.Noop => value
      case Instruction.AddX(x) =>
        val r = value
        value += x
        r


@main def run_part1 =
  println(summarizeResource("day10_data.txt") { lines =>
    val values = execute(lines.map(parse_line))

    var total = 0
    for i <- 21 until (220 + 21) do
      val v = values.next()
      if i % 40 == 0 then total += v*(i - 20)

    total
  })

@main def run_part2 =
  println(summarizeResource("day10_data.txt") { lines =>
    val values = execute(lines.map(parse_line))

    val sb = StringBuilder()
    for i <- 0 until 240 do
      if i % 40 == 0 then sb.addOne('\n')
      val v = values.next()
      if (v - (i % 40)).abs <= 1 then sb.addOne('#') else sb.addOne('.')

    sb
  })
