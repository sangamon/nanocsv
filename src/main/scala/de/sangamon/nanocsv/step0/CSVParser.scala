package de.sangamon.nanocsv.step0

import java.nio.file.*
import java.time.*
import scala.io.*
import scala.util.*

type Row = List[String]

object CSVParser:

  val string: String => String = identity
  val int: String => Int = _.toInt
  val date: String => LocalDate = LocalDate.parse

  private def lines(file: Path): List[String] =
    Using.resource(Source.fromFile(file.toFile)) {
      _.getLines().toList
    }

  private def row(line: String): Row = line.split(',').toList

  def parse[T](file: Path)(conv: Row => T): List[T] =
    lines(file).map(l => conv(row(l)))
