package de.sangamon.nanocsv.step05

import cats.*
import cats.syntax.all.*

import java.io.FileNotFoundException
import java.nio.file.*
import java.time.*
import scala.io.*
import scala.util.*

type Row = List[String]

enum CSVParseFailure:
  case ColumnParseFailure(cause: Throwable)
  case RowExhaustionFailure

type CSVResult[T] = Either[CSVParseFailure, T]

trait RowParser[T]:
  def parse(row: Row): CSVResult[(T, Row)]

object RowParser:

  import CSVParseFailure.*

  given Applicative[RowParser] with
    def pure[A](x: A): RowParser[A] = (x, _).pure
    def ap[A, B](ff: RowParser[A => B])(fa: RowParser[A]): RowParser[B] =
      row =>
        for {
          (f, fr) <- ff.parse(row)
          (a, ar) <- fa.parse(fr)
        } yield f(a) -> ar

  extension[A](p: RowParser[A])
    def emap[B](f: A => CSVResult[B]): RowParser[B] =
      p.parse(_) >>= { case (res, rem) => f(res).map(_ -> rem) }
    def guardMap[B](f: A => B): RowParser[B] =
      emap { a => Either.catchNonFatal(f(a)).leftMap(ColumnParseFailure(_)) }

  val string: RowParser[String] =
    case h :: t => (h, t).pure
    case Nil => RowExhaustionFailure.asLeft[(String, Row)]

  val int: RowParser[Int] = string.guardMap(_.toInt)
  val date: RowParser[LocalDate] = string.guardMap(LocalDate.parse)

  val end: RowParser[Unit] = {
    case Nil => ((), Nil).asRight
    case _ => ColumnParseFailure(new IllegalStateException("trailing data")).asLeft
  }

  given RowParser[String] = string
  given RowParser[Int] = int
  given RowParser[LocalDate] = date

trait RowParserDerivable[A, B]:
  def deriveRowParser(a: A): RowParser[B]

object RowParserDerivable:

  given[A, B](using RowParser[A]): RowParserDerivable[A => B, B] with
    def deriveRowParser(f: A => B): RowParser[B] = summon[RowParser[A]].map(f)

  given[A, B, C](using RowParser[A], RowParserDerivable[B, C]): RowParserDerivable[A => B, C] with
    def deriveRowParser(f: A => B): RowParser[C] =
      row =>
        for {
          (a, remA) <- summon[RowParser[A]].parse(row)
          res <- summon[RowParserDerivable[B, C]].deriveRowParser(f(a)).parse(remA)
        } yield res

  extension[A](a: A)
    def deriveRowParser[B](using derive: RowParserDerivable[A, B]): RowParser[B] = derive.deriveRowParser(a)

case class CSVIOFailure(file: Path, cause: Throwable)

type CSVFailure = CSVIOFailure | CSVParseFailure

object CSVParser:

  import CSVParseFailure.*

  private def lines(file: Path): Either[CSVIOFailure, List[String]] =
    Either
      .catchOnly[FileNotFoundException] {
        Using.resource(Source.fromFile(file.toFile)) { _.getLines().toList }
      }
      .leftMap(CSVIOFailure(file, _))

  private def row(line: String): Row = line.split(',').toList

  private def parseRow[T](p: RowParser[T])(row: Row): CSVResult[T] = p.parse(row).map(_(0))

  def parseLines[T](p: RowParser[T])(lines: List[String]): CSVResult[List[T]] =
    lines.map(row).traverse(parseRow(p))

  def parse[T](file: Path)(p: RowParser[T]): Either[CSVFailure, List[T]] =
    lines(file).leftWiden[CSVFailure] >>= parseLines(p)
