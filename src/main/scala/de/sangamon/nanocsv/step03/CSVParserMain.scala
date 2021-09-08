package de.sangamon.nanocsv.step03

import cats.syntax.all.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*
import java.time.LocalDate

object CSVParserMain:

  import CSVParser.*
  import RowParser.*
  import RowParserDerivable.*

  val userParser: RowParser[User] = User.apply.curried.deriveRowParser

  @main def main(csvFile: String): Unit =
    for(u <- CSVParser.parse(Paths.get(csvFile))(userParser))
      println(u)
