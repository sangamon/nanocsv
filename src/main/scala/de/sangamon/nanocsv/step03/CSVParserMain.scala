package de.sangamon.nanocsv.step03

import cats.syntax.all.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*
import java.time.*
import scala.io.*
import scala.util.*

object CSVParserMain:

  import CSVParser.*
  import RowParser.*
  import RowParserDerivable.*

  val userParser: RowParser[User] = User.apply.curried.deriveRowParser

  @main def main(csvFile: String): Unit =
    Using(Source.fromFile(csvFile)) { _.getLines().foreach(println) }
    CSVParser.parse(Paths.get(csvFile))(userParser).foreach(println)
