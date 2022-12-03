package aoc22.day2_basic

import aoc22.summarizeResource


enum RPS:
  case Rock, Paper, Scissors

  infix def compare(other: RPS): Int = (this, other) match
    case (x, y) if x == y => 0
    case (Rock, Scissors) => 1
    case (Paper, Rock) => 1
    case (Scissors, Paper) => 1
    case _ => -1

  infix def complement(result: Int): RPS = (this, result) match
    case (x, 0) => x
    case (Rock, 1) => Scissors
    case (Paper, 1) => Rock
    case (Scissors, 1) => Paper
    case (x, -1) => (x complement 1) complement 1
import RPS.*


val left_options = "ABC"
val right_options = "XYZ"
val line_pattern = (s"([$left_options]) ([$right_options])").r

def parse_line(line: String): (Int, Int) = line match
  case line_pattern(left, right) => left_options.indexOf(left) -> right_options.indexOf(right)
  case _ => throw RuntimeException(s"Invalid line: $line")


extension (m: RPS)
  def valuation: Int = m match
    case Rock => 1
    case Paper => 2
    case Scissors => 3

extension (i: Int)
  def valuation: Int = i match
    case -1 => 0
    case 0 => 3
    case 1 => 6


def score(pair: (Int, Int)) =
  val opponent_move = RPS.fromOrdinal(pair._1)
  val you_move = RPS.fromOrdinal(pair._2)
  val choice_score = you_move.valuation
  val game_score = (you_move compare opponent_move).valuation
  choice_score + game_score

def real_score(pair: (Int, Int)) =
  val opponent_move = RPS.fromOrdinal(pair._1)
  val game_result = pair._2 - 1
  val choice_score = (opponent_move complement -game_result).valuation
  val game_score = game_result.valuation
  choice_score + game_score


@main def run_part1 =
  println(summarizeResource("day2_data.txt")(_.map(parse_line).map(score).sum))

@main def run_part2 =
  println(summarizeResource("day2_data.txt")(_.map(parse_line).map(real_score).sum))
