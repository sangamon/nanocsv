package de.sangamon.nanocsv.step02

import cats.syntax.all.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*

object CSVParserMain:

  import CSVParser.*
  import RowParser.*

  val userParser: RowParser[User] = (int, string, date).mapN(User.apply)

  @main def main(csvFile: String): Unit =
    for(u <- CSVParser.parse(Paths.get(csvFile))(userParser))
      println(u)
