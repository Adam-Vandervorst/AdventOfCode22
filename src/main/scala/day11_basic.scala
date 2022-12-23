package aoc22.day11_basic

import aoc22.summarizeResource


object UIntList:
  def unapply(s: String): Option[Seq[Int]] =
    Some(s.split(", ").map(_.toInt))

enum MonkeyExpr:
  case Square
  case Mul(x: Int)
  case Add(x: Int)

  def apply(i: Long): Long = this match
    case Square => i * i
    case Mul(x) => i * x
    case Add(x) => i + x

object MonkeyExpr:
  val square_format = raw"old \* old".r
  val mul_format = raw"old \* (\d+)".r
  val add_format = raw"old \+ (\d+)".r
  def unapply(s: String): Option[MonkeyExpr] = s match
    case square_format() => Some(Square)
    case mul_format(x) => Some(Mul(x.toInt))
    case add_format(x) => Some(Add(x.toInt))
    case _ => None

case class MonkeyPolicy(divby: Int, ontrue: Int, onfalse: Int):
  def apply(i: Long): Int =
    if i % divby == 0 then ontrue else onfalse

case class Monkey(number: Int, items: Seq[Int], operation: MonkeyExpr, policy: MonkeyPolicy)

val number_format = raw"Monkey (\d+):".r
val items_format = raw"  Starting items: (\d+(?:, \d+)*)".r
val operation_format = raw"  Operation: new = (.+)".r
val condition_format = raw"  Test: divisible by (\d+)".r
val ontrue_format = raw"    If true: throw to monkey (\d+)".r
val onfalse_format = raw"    If false: throw to monkey (\d+)".r
def read_monkey(lines: Iterator[String]): Monkey =
  val number_format(number) = lines.next()
  val items_format(UIntList(items)) = lines.next()
  val operation_format(MonkeyExpr(operation)) = lines.next()
  val condition_format(divby) = lines.next()
  val ontrue_format(ontrue) = lines.next()
  val onfalse_format(onfalse) = lines.next()
  val monkey_policy = MonkeyPolicy(divby.toInt, ontrue.toInt, onfalse.toInt)
  Monkey(number.toInt, items, operation, monkey_policy)

def read_monkeys(lines: Iterator[String]): Array[Monkey] =
  val without_blank = lines.filterNot(_.isBlank)
  val monkeys = Array.newBuilder[Monkey]

  while without_blank.hasNext do
    val new_monkey = read_monkey(without_blank)
    assert(new_monkey.number == monkeys.length)
    monkeys.addOne(new_monkey)

  monkeys.result()

def play(monkeys: Array[Monkey], rounds: Int, discount: Int): Array[Long] =
  val modAll = monkeys.map(_.policy.divby).product
  assert(modAll < Int.MaxValue)
  val monkey_items = monkeys.map(_.items.map(_.toLong).to(collection.mutable.ArrayDeque))
  val monkey_counts = monkeys.map(_ => 0L)

  for _ <- 1 to rounds
      monkey <- monkeys
      item <- monkey_items(monkey.number).removeAll() do
    monkey_counts(monkey.number) += 1
    val worry_after_inspection = monkey.operation(item) % modAll
    val worry_after_realization = worry_after_inspection/discount
    val throw_to_monkey = monkey.policy(worry_after_realization)
    monkey_items(throw_to_monkey).append(worry_after_realization)

  monkey_counts

def monkey_business(counts: Array[Long]): Long =
  counts.sortInPlace().view.reverse.take(2).product

@main def run_part1 =
  println(summarizeResource("day11_data.txt") { lines =>
    monkey_business(play(read_monkeys(lines), 20, 3))
  })

@main def run_part2 =
  println(summarizeResource("day11_data.txt") { lines =>
    monkey_business(play(read_monkeys(lines), 10000, 1))
  })
