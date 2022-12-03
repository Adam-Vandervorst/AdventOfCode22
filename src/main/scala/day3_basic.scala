package day3_basic

import java.lang.Long.numberOfTrailingZeros
import scala.io.Source
import scala.util.Using


def misplaced(s: String): Int =
  val half = s.length/2
  var ls = 0L
  var rs = 0L
  for i <- 0 until half do
    ls |= 2L << score(s(i))
  for i <- half until s.length do
    rs |= 2L << score(s(i))

  numberOfTrailingZeros(ls & rs) - 1

def score(i: Char): Long =
  if i < 97 then i - (64 - 26)
  else i - 96


@main def run =
  Using(Source.fromFile("src/main/resources/day3_example.txt"))(f =>
    println(f.getLines().map(misplaced).sum)
  ).get
