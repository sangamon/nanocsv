package de.sangamon.nanocsv.step02

import cats.syntax.all.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*
import scala.io.*
import scala.util.*

object CSVParserMain:

  import CSVParser.*
  import RowParser.*

  val userParser: RowParser[User] = (int, string, date).mapN(User.apply)

  @main def main(csvFile: String): Unit =
    Using(Source.fromFile(csvFile)) { _.getLines().foreach(println) }
    CSVParser.parse(Paths.get(csvFile))(userParser).foreach(println)
