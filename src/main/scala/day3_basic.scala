package day3_basic

import java.lang.Long.numberOfTrailingZeros
import scala.io.Source
import scala.util.Using


def splitEqual(s: String): List[String] =
  val half = s.length/2
  List(s.take(half), s.drop(half))

def misplaced(gs: Seq[String]): Int =
  numberOfTrailingZeros(gs
    .map(_.foldRight(0L)((c, t) => t | (2L << score(c))))
    .reduce(_ & _)) - 1

def score(i: Char): Long =
  if i < 97 then i - (65 - 1 - 26)
  else i - (97 - 1)


@main def run =
  Using(Source.fromFile("src/main/resources/day3_data.txt"))(f =>
    println(f.getLines().map(splitEqual).map(misplaced).sum)
  ).get
