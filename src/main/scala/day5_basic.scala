package aoc22.day5_basic

import aoc22.summarizeResource

import scala.collection.mutable

val stack_block = raw"\[(.)\]|    ".r
def read_stacks(lines: Iterator[String]): Array[mutable.ArrayDeque[Char]] =
  val stacks = mutable.ArrayDeque.empty[mutable.ArrayDeque[Char]]
  for line <- lines
    (s, i) <- stack_block.findAllMatchIn(line).flatMap(_.subgroups).zipWithIndex do
      if i >= stacks.length then stacks.append(mutable.ArrayDeque.empty)
      if s != null then stacks(i).addOne(s.head)
  stacks.toArray

val move_description = raw"move (\d+) from (\d) to (\d)".r
def read_move(line: String): (Int, Int, Int) = line match
  case move_description(n, i, j) => (n.toInt, i.toInt - 1, j.toInt - 1)
  case _ => throw RuntimeException(s"Invalid line: $line")

def execute(stacks: Array[mutable.ArrayDeque[Char]], moves: Iterator[(Int, Int, Int)], multiple: Boolean): Unit =
  for (n, i, j) <- moves do
    val src_stack = stacks(i)
    val dst_stack = stacks(j)
    if multiple then dst_stack.prependAll(src_stack.view.take(n))
    else dst_stack.prependAll(src_stack.view.take(n).reverse)
    src_stack.dropInPlace(n)

def tops(stacks: Seq[mutable.ArrayDeque[Char]]): Seq[Char] =
  stacks.map(_.head)


@main def run_part1 =
  println(summarizeResource("day5_data.txt")(lines => {
    val stacks = read_stacks(lines.takeWhile(!_.isBlank))
    println(stacks.toList)
    execute(stacks, lines.map(read_move), multiple=false)
    tops(stacks).mkString
  }))

@main def run_part2 =
  println(summarizeResource("day5_data.txt")(lines =>
    val stacks = read_stacks(lines.takeWhile(!_.isBlank))
    execute(stacks, lines.map(read_move), multiple=true)
    tops(stacks).mkString
  ))