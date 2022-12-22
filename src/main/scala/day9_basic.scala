package aoc22.day9_basic

import aoc22.summarizeResource


type Pos = (Int, Int)

object Pos:
  def fromLetter(c: Char): Pos = c match
    case 'L' => (-1, 0)
    case 'U' => (0, 1)
    case 'D' => (0, -1)
    case 'R' => (1, 0)

extension (x: Pos)
  def norm : Double = Math.sqrt(x._1*x._1 + x._2*x._2)
  def +(y: Pos): Pos = (x._1 + y._1, x._2 + y._2)
  def -(y: Pos): Pos = (x._1 - y._1, x._2 - y._2)
  def /(n: Int): Pos = (x._1/n, x._2/n)


val line_pattern = raw"([LUDR]) (\d+)".r
def parse_line(line: String): (Pos, Int) = line match
  case line_pattern(dir, count) => Pos.fromLetter(dir.head) -> count.toInt
  case _ => throw RuntimeException(s"Invalid line: $line")

def execute(instructions: Iterator[(Pos, Int)], length: Int): Iterator[Pos] =
  val positions = Array.fill(length)((0, 0))

  for case (offset, steps) <- instructions
      _ <- 1 to steps yield

    positions(0) += offset
    for i <- 1 until length do
      positions(i) = positions(i - 1) + follow(positions(i) - positions(i - 1))

    positions.last

def follow(diff: Pos): Pos =
  if diff.norm >= 2 then diff / 2 else diff


@main def run_part1 =
  println(summarizeResource("day9_data.txt") { lines =>
    val positions = execute(lines.map(parse_line), 2).toSet
    positions.size
  })

@main def run_part2 =
  println(summarizeResource("day9_data.txt") { lines =>
    val positions = execute(lines.map(parse_line), 10).toSet
    positions.size
  })
