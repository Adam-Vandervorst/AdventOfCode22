package aoc22.day1_basic

import aoc22.summarizeResource


def low_level_max(it: Iterator[String]): Int =
  var supr = 0
  var curr = 0

  for line <- it do
    if line == "" then
      supr = curr max supr
      curr = 0
    else
      curr += line.toInt

  curr max supr


def low_level_top(it: Iterator[String], n: Int): Int =
  val top = collection.mutable.ArrayDeque.fill(n)(0)
  var cur = 0

  def update_top(i: Int = 0): Unit =
    if i < n && cur > top(i) then update_top(i + 1)
    else if i > 0 then
      top.insert(i, cur)
      top.dropInPlace(1)

  for line <- it do
    if line == "" then
      update_top()
      cur = 0
    else
      cur += line.toInt
  update_top()

  top.sum


@main def run_part1 =
  println(summarizeResource("day1_data.txt")(low_level_max))

@main def run_part2 =
  println(summarizeResource("day1_data.txt")(low_level_top(_, 3)))
