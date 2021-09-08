package de.sangamon.nanocsv.step02

import cats.*
import cats.syntax.all.*
import java.nio.file.*
import java.time.*
import scala.io.*
import scala.util.*

type Row = List[String]

class CSVException(msg: String) extends IllegalArgumentException(msg)

trait RowParser[T]:
  def parse(row: Row): (T, Row)

object RowParser:

  given Applicative[RowParser] with
    def pure[A](x: A): RowParser[A] = (x, _)
    def ap[A, B](ff: RowParser[A => B])(fa: RowParser[A]): RowParser[B] =
      row => {
        val (f, fr) = ff.parse(row)
        val (a, ar) = fa.parse(fr)
        f(a) -> ar
      }

  val string: RowParser[String] =
    case h :: t => h -> t
    case Nil => throw new CSVException("input exhausted")

  val int: RowParser[Int] = string.map(_.toInt)
  val date: RowParser[LocalDate] = string.map(LocalDate.parse)

object CSVParser:

  private def lines(file: Path): List[String] =
    Using.resource(Source.fromFile(file.toFile)) {
      _.getLines().toList
    }

  private def row(line: String): Row = line.split(',').toList

  def parse[T](file: Path)(p: RowParser[T]): List[T] =
    lines(file).map(l => p.parse(row(l))._1)
