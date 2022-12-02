/*
Part 1.
The Elves begin to set up camp on the beach. To decide whose tent gets to be closest to the snack storage, a giant Rock Paper Scissors tournament is already in progress.

Rock Paper Scissors is a game between two players. Each game contains many rounds; in each round, the players each simultaneously choose one of Rock, Paper, or Scissors using a hand shape. Then, a winner for that round is selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. If both players choose the same shape, the round instead ends in a draw.

Appreciative of your help yesterday, one Elf gives you an encrypted strategy guide (your puzzle input) that they say will be sure to help you win. "The first column is what your opponent is going to play: A for Rock, B for Paper, and C for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's tent.

The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors. Winning every time would be suspicious, so the responses must have been carefully chosen.

The winner of the whole tournament is the player with the highest score. Your total score is the sum of your scores for each round. The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

Since you can't be sure if the Elf is trying to help you or trick you, you should calculate the score you would get if you were to follow the strategy guide.

For example, suppose you were given the following strategy guide:

A Y
B X
C Z
This strategy guide predicts and recommends the following:

In the first round, your opponent will choose Rock (A), and you should choose Paper (Y). This ends in a win for you with a score of 8 (2 because you chose Paper + 6 because you won).
In the second round, your opponent will choose Paper (B), and you should choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
The third round is a draw with both players choosing Scissors, giving you a score of 3 + 3 = 6.
In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6).

What would your total score be if everything goes exactly according to your strategy guide?
 */

package day2_basic

import scala.io.Source
import scala.util.Using


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


@main def run =
  Using(Source.fromFile("src/main/resources/day2_data.txt"))(f =>
    println(f.getLines().map(parse_line).map(real_score).sum)
  ).get
