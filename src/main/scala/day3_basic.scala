package aoc22.day3_basic

import aoc22.summarizeResource

import java.lang.Long.numberOfTrailingZeros


def split_equal(s: String): List[String] =
  val half = s.length/2
  List(s.take(half), s.drop(half))

def misplaced(gs: Seq[String]): Int =
  numberOfTrailingZeros(gs
    .map(_.foldRight(0L)((c, t) => t | (2L << score(c))))
    .reduce(_ & _)) - 1

def score(i: Char): Long =
  if i < 97 then i - (65 - 1 - 26)
  else i - (97 - 1)


@main def run_part1 =
  println(summarizeResource("day3_data.txt")(_.map(split_equal).map(misplaced).sum))

@main def run_part2 =
  println(summarizeResource("day3_data.txt")(_.grouped(3).map(misplaced).sum))
