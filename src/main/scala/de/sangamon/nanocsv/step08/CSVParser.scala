package de.sangamon.nanocsv.step08

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.*

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

type CSVParseEff[T] = StateT[CSVResult, ParserState, T]

trait RowParser[T]:
  def parse(): CSVParseEff[T]

object RowParser:

  import CSVParseFailure.*

  given Applicative[RowParser] with
    def pure[A](a: A): RowParser[A] = () => a.pure
    def ap[A, B](ff: RowParser[A => B])(fa: RowParser[A]): RowParser[B] =
      () =>
        for {
          f <- ff.parse()
          a <- fa.parse()
        } yield f(a)

  extension[A](p: RowParser[A])
    def emap[B](f: A => CSVParseEff[B]): RowParser[B] =
      () => p.parse() >>= f
    def guardMap[B](f: A => B): RowParser[B] =
      emap {
        a => StateT.inspectF { s =>
          Either.catchNonFatal(f(a)).leftMap(ColumnParseFailure(_, ParserPos(s.pos.rowIdx, s.prevColIdx)))
        }
      }

  val string: RowParser[String] =
    () =>
      StateT {
        case ParserState(ParserPos(r, c), _, h :: t) => (ParserState(ParserPos(r, c + 1), c, t), h).pure
        case ParserState(p, _, Nil) => RowExhaustionFailure(p).asLeft
      }

  val int: RowParser[Int] = string.guardMap(_.toInt)
  val date: RowParser[LocalDate] = string.guardMap(LocalDate.parse)

  val end: RowParser[Unit] =
    () =>
      StateT.inspectF {
        case s@ParserState(_, _, Nil) => ().pure
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
      () =>
        for {
          a <- summon[RowParser[A]].parse()
          c <- summon[RowParserDerivable[B, C]].deriveRowParser(f(a)).parse()
        } yield c

  extension[A](a: A)
    def deriveRowParser[B](using derive: RowParserDerivable[A, B]): RowParser[B] = derive.deriveRowParser(a)

case class CSVIOFailure(file: Path, cause: Throwable)

type CSVFailure = CSVIOFailure | CSVParseFailure

object CSVParser:

  import CSVParseFailure.*

  private def lines(file: Path): IO[Either[CSVIOFailure, List[String]]] =
    Resource
      .fromAutoCloseable { IO.blocking { Source.fromFile(file.toFile) } }
      .use { src => IO.blocking { src.getLines().toList.asRight } }
      .recover {
        case fnfExc: FileNotFoundException =>
          CSVIOFailure(file.toAbsolutePath, fnfExc).asLeft
      }

  private def row(line: String): Row = line.split(',').toList

  private def parseRow[T](p: RowParser[T])(row: Row, rowIdx: Int): CSVResult[T] =
    p.parse().runA(ParserState(ParserPos(rowIdx, 0), 0, row))

  def parseLines[T](p: RowParser[T])(lines: List[String]): CSVResult[List[T]] =
    lines.map(row).zipWithIndex.traverse(parseRow(p))

  def parse[T](file: Path)(p: RowParser[T]): IO[Either[CSVFailure, List[T]]] =
    lines(file).map(_.leftWiden[CSVFailure] >>= parseLines(p))
