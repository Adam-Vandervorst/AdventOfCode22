package aoc22.day6_basic

import aoc22.summarizeResource

import scala.collection.mutable


def index_of_unique(s: String, window: Int): Int =
  val running_set = mutable.MultiSet.from(s.take(window))
  var i = 0

  while i < s.length - window do
    if running_set.occurrences.size == window then
      return i + window
    running_set.subtractOne(s(i))
    running_set.addOne(s(i + window))
    i += 1
  -1


@main def run_part1 =
  println(summarizeResource("day6_data.txt")(lines =>
    index_of_unique(lines.next(), 4)
  ))

@main def run_part2 =
  println(summarizeResource("day6_data.txt")(lines =>
    index_of_unique(lines.next(), 14)
  ))
