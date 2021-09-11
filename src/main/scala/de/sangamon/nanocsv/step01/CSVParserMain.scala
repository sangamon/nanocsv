package de.sangamon.nanocsv.step01

import de.sangamon.nanocsv.shared.*

import java.nio.file.*
import scala.io.*
import scala.util.*

object CSVParseMain:

  import CSVParser.*
  import RowParser.*

  val userParser: RowParser[User] =
    const(User.apply.curried) <*> int <*> string <*> date

  @main def main(csvFile: String): Unit =
    Using(Source.fromFile(csvFile)) { _.getLines().foreach(println) }
    CSVParser.parse(Paths.get(csvFile))(userParser).foreach(println)
