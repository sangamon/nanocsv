package de.sangamon.nanocsv.step01

import java.nio.file.*
import java.time.*
import scala.io.*
import scala.util.*

type Row = List[String]

class CSVException(msg: String) extends IllegalArgumentException(msg)

trait RowParser[T]:
  def parse(row: Row): (T, Row)

object RowParser:

  def const[T](v: T): RowParser[T] = (v, _)

  def transform[A, B](p: RowParser[A])(f: A => B): RowParser[B] =
    row =>
      val (a, ar) = p.parse(row)
      (f(a), ar)

  def combine[A, B](fp: RowParser[A => B], p: RowParser[A]): RowParser[B] =
    row =>
      val (f, fr) = fp.parse(row)
      val (v, vr) = p.parse(fr)
      f(v) -> vr

  extension[A, B](fp: RowParser[A => B])
    def <*>(p: RowParser[A]): RowParser[B] = combine(fp, p)

  val string: RowParser[String] =
    case h :: t => h -> t
    case Nil => throw new CSVException("input exhausted")
  
  val int: RowParser[Int] = transform(string)(_.toInt)
  val date: RowParser[LocalDate] = transform(string)(LocalDate.parse)
  
object CSVParser:

  private def lines(file: Path): List[String] =
    Using.resource(Source.fromFile(file.toFile)) {
      _.getLines().toList
    }

  private def row(line: String): Row = line.split(',').toList

  def parse[T](file: Path)(p: RowParser[T]): List[T] =
    lines(file).map(l => p.parse(row(l))(0))
