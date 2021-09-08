package de.sangamon.nanocsv.step01

import de.sangamon.nanocsv.shared._

import java.nio.file.*

object CSVParseMain:

  import CSVParser.*
  import RowParser.*

  val userParser: RowParser[User] =
    const(User.apply.curried) <*> int <*> string <*> date

  @main def main(csvFile: String): Unit =
    for(u <- CSVParser.parse(Paths.get(csvFile))(userParser))
      println(u)
