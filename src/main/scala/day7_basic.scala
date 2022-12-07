package aoc22.day7_basic

import aoc22.summarizeResource


enum Command:
  case ToHomeDir
  case UpOneDir
  case SelectDir(name: String)
  case ListAll
import Command.*

enum Output:
  case File(name: String, size: Int)
  case Dir(name: String)
import Output.*

case class RecMap[K, V](rec: Map[K, RecMap[K, V]],
                        plain: Map[K, V]):
  def updatedAt(cursor: Seq[K], v: V): RecMap[K, V] = cursor match
    case Seq(fst, snd, rest: _*) => RecMap(rec.updatedWith(fst)(o =>
      Some(o.getOrElse(RecMap.empty).updatedAt(snd +: rest, v))), plain)
    case Seq(last) => RecMap(rec, plain.updated(last, v))

  def fold[T](f: (Map[K, T], Map[K, V]) => T): T =
    f(rec.map((k, v) => k -> v.fold(f)), plain)

object RecMap:
  def empty[K, V]: RecMap[K, V] = RecMap(Map.empty, Map.empty)

type DirSizeTree = RecMap[String, Int]


val command_pattern = ("\\$" + raw" (cd|ls) ?(\/|\.\.|[a-z]+)?").r
val output_pattern = raw"(dir|\d+) (.+)".r
def parse_line(line: String): Either[Command, Output] = line match
  case command_pattern("cd", arg) => Left(arg match
    case "/" => ToHomeDir
    case ".." => UpOneDir
    case name => SelectDir(name))
  case command_pattern("ls", _) => Left(ListAll)
  case output_pattern(info, name) => Right(info match
    case "dir" => Dir(name)
    case size => File(name, size.toInt))
  case _ => throw RuntimeException(s"Invalid line: $line")

def integrate(state: (DirSizeTree, Seq[String]), action: Either[Command, Output]): (DirSizeTree, Seq[String]) =
  val (tree, path) = state
  action match
    case Left(cmd) => cmd match
      case ToHomeDir => tree -> Vector.empty
      case UpOneDir => tree -> path.dropRight(1)
      case SelectDir(name) => tree -> path.appended(name)
      case ListAll => state
    case Right(out) => out match
      case File(name, size) => tree.updatedAt(path.appended(name), size) -> path
      case Dir(name) => state

def parse_dir_tree(description: Iterator[String]): DirSizeTree =
  description.map(parse_line).foldLeft((RecMap.empty, Nil))(integrate)._1


@main def run_part1 =
  println(summarizeResource("day7_data.txt") { lines =>
    val dir_tree = parse_dir_tree(lines)
    dir_tree.fold[(Int, Int)]((processed_dirs, sizes) =>
      val total = (processed_dirs.values.map(_._1) ++ sizes.values).sum
      val total_under_100k = processed_dirs.values.map(_._2).sum
      total -> (if total <= 100_000 then total + total_under_100k else total_under_100k)
    )._2
  })

@main def run_part2 =
  println(summarizeResource("day7_data.txt") { lines =>
    val dir_tree = parse_dir_tree(lines)
    val required = dir_tree.fold[Int]((processed_dirs, sizes) =>
      processed_dirs.values.sum + sizes.values.sum
    ) - 40_000_000
    dir_tree.fold[(Int, Option[Int])]((processed_dirs, sizes) =>
      val total = processed_dirs.values.map(_._1).sum + sizes.values.sum
      total -> Option.unless(total < required)(processed_dirs.values.foldLeft(total) {
        case (m, (_, Some(k))) if k < m => k
        case (m, _) => m
      })
    )._2
  })
