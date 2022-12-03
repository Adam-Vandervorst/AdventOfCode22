package aoc22

import scala.io.Source
import scala.util.Using


def summarizeResource[T](filename: String)(summarize: Iterator[String] => T): T =
  Using(Source.fromFile(s"src/main/resources/$filename"))(f =>
    summarize(f.getLines())
  ).get
