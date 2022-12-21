package aoc22.day9_basic

import aoc22.summarizeResource


type Pos = (Int, Int)

enum Direction:
  case Left, Up, Down, Right

  def offset: Pos = this match
    case Left => (-1, 0)
    case Up => (0, 1)
    case Down => (0, -1)
    case Right => (1, 0)

object Direction:
  def fromLetter(c: Char): Direction = c match
    case 'L' => Left
    case 'U' => Up
    case 'D' => Down
    case 'R' => Right

extension (x: Pos)
  def unary_- : Pos = (-x._1, -x._2)
  def +(y: Pos): Pos = (x._1 + y._1, x._2 + y._2)
  def -(y: Pos): Pos = (x._1 - y._1, x._2 - y._2)
  infix def dot(y: Pos): Int = x._1 * y._1 + x._2 * y._2

val line_pattern = raw"([LUDR]) (\d+)".r
def parse_line(line: String): (Direction, Int) = line match
  case line_pattern(dir, count) => Direction.fromLetter(dir.head) -> count.toInt
  case _ => throw RuntimeException(s"Invalid line: $line")

def execute(instructions: Iterator[(Direction, Int)]): Iterator[Pos] =
  var head_pos = (0, 0)
  var relative_tail_pos = (0, 0)

  instructions.flatMap((d, steps) =>
    val offset = d.offset
    (1 to steps).map{ _ =>
      head_pos = head_pos + offset
      relative_tail_pos = follow(relative_tail_pos, offset)
      head_pos + relative_tail_pos
    }
  )

def follow(relative_tail_pos: Pos, offset: Pos): Pos =
  // TH    ==>
  //  TH
  // (-1, 0),  (1, 0)
  // (-1, 0)
  if relative_tail_pos == -offset then
    relative_tail_pos
  // TH    =>
  // T
  //  H
  // (-1, 0),  (0, -1)
  // (-1, 1)
  //   T
  //  H    =>
  //   T
  //   H
  // (1, 1), (1, 0)
  // (0, 1)
  else if (relative_tail_pos dot offset) >= 0 then
    relative_tail_pos - offset
  // T
  //  H    =>
  //  TH
  // (-1, 1),  (1, 0)
  // (-1, 0)
  else
    -offset

@main def run_part1 =
  println(summarizeResource("day9_data.txt") { lines =>
    val positions = execute(lines.map(parse_line)).toSet
    positions.size
  })

@main def run_part2 =
  ???
