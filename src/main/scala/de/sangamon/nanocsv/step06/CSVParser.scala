package de.sangamon.nanocsv.step06

import cats.*
import cats.data.*
import cats.syntax.all.*

import java.io.*
import java.nio.file.*
import java.time.*
import scala.io.*
import scala.util.*

type Row = List[String]

case class ParserPos(rowIdx: Int, colIdx: Int)

enum CSVParseFailure:
  case ColumnParseFailure(cause: Throwable, pos: ParserPos)
  case RowExhaustionFailure(pos: ParserPos)

type CSVResult[T] = Either[CSVParseFailure, T]

case class ParserState(pos: ParserPos, prevColIdx: Int, remainder: Row)

trait RowParser[T]:
  def parse(st: ParserState): CSVResult[(T, ParserState)]

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
      p.parse(_) >>= { case (res, st) => f(res).map(_ -> st) }
    def guardMap[B](f: A => B): RowParser[B] =
      p.parse(_) >>= {
        case (res, st@ParserState(ParserPos(r, c), pc, _)) =>
          Either.catchNonFatal(f(res)).leftMap(ColumnParseFailure(_, ParserPos(r, pc))).map(_ -> st)
        }

  val string: RowParser[String] =
    case ParserState(ParserPos(r, c), _, h :: t) => (h, ParserState(ParserPos(r, c + 1), c, t)).pure
    case ParserState(p, _, Nil) => RowExhaustionFailure(p).asLeft[(String, ParserState)]

  val int: RowParser[Int] = string.guardMap(_.toInt)
  val date: RowParser[LocalDate] = string.guardMap(LocalDate.parse)

  val end: RowParser[Unit] = {
    case s@ParserState(_, _, Nil) => ((), s).asRight
    case ParserState(pos, _, _ :: _) =>
      ColumnParseFailure(new IllegalStateException("trailing data"), pos).asLeft
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

  private def parseRow[T](p: RowParser[T])(row: Row, rowIdx: Int): CSVResult[T] =
    p.parse(ParserState(ParserPos(rowIdx, 0), 0, row)).map(_(0))

  def parseLines[T](p: RowParser[T])(lines: List[String]): CSVResult[List[T]] =
    lines.map(row).zipWithIndex.traverse(parseRow(p))

  def parse[T](file: Path)(p: RowParser[T]): Either[CSVFailure, List[T]] =
    lines(file).leftWiden[CSVFailure] >>= parseLines(p)
