package de.sangamon.nanocsv.step00

import de.sangamon.nanocsv.shared.User

import java.nio.file.*

object CSVParserMain:

  import CSVParser.*

  val parseUser: Row => User =
    case List(i, n, b) => User(int(i), string(n), date(b))
    case _ => throw new IllegalArgumentException("wrong number of columns for user")

  @main def main(csvFile: String): Unit =
    for(u <- CSVParser.parse(Paths.get(csvFile))(parseUser))
      println(u)
