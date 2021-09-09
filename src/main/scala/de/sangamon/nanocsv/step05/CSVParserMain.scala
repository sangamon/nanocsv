package de.sangamon.nanocsv.step05

import cats.syntax.all.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*

object CSVParseMain:

  import RowParser.*
  import RowParserDerivable.*

  val userParser: RowParser[User] =
    User.apply.curried.deriveRowParser[User] <* end

  @main def main(csvFile: String): Unit =
    CSVParser.parse(Paths.get(csvFile))(userParser) match
      case Left(f) => println(s"ERROR: $f")
      case Right(r) => r.foreach(println)
