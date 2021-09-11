package de.sangamon.nanocsv.step00

import de.sangamon.nanocsv.shared.User

import java.nio.file.*
import scala.io.*
import scala.util.*

object CSVParserMain:

  import CSVParser.*

  val userParser: Row => User =
    case List(i, n, b) => User(int(i), string(n), date(b))
    case _ => throw new IllegalArgumentException("wrong number of columns for user")
  
  @main def main(csvFile: String): Unit =
    Using(Source.fromFile(csvFile)) { _.getLines().foreach(println) }
    CSVParser.parse(Paths.get(csvFile))(userParser).foreach(println)
