package aoc22.day4_basic

import aoc22.summarizeResource


val line_pattern = raw"(\d+)-(\d+),(\d+)-(\d+)".r

def parse_line(line: String): ((Int, Int), (Int, Int)) = line match
  case line_pattern(l1, h1, l2, h2) => (l1.toInt, h1.toInt) -> (l2.toInt, h2.toInt)
  case _ => throw RuntimeException(s"Invalid line: $line")

def fully_contains(intervals: ((Int, Int), (Int, Int))): Boolean =
  val ((l1, h1), (l2, h2)) = intervals
  (l1 <= l2 && h2 <= h1) ||
  (l2 <= l1 && h1 <= h2)


def overlaps(intervals: ((Int, Int), (Int, Int))): Boolean =
  val ((l1, h1), (l2, h2)) = intervals
  !(h1 < l2 || h2 < l1)


@main def run_part1 =
  println(summarizeResource("day4_data.txt")(_.map(parse_line).count(fully_contains)))

@main def run_part2 =
  println(summarizeResource("day4_data.txt")(_.map(parse_line).count(overlaps)))
