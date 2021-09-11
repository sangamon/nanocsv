package de.sangamon.nanocsv.step07

import cats.syntax.all.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*
import scala.io.*
import scala.util.*

object CSVParseMain:

  import RowParser.*
  import RowParserDerivable.*

  val userParser: RowParser[User] =
    User.apply.curried.deriveRowParser[User] <* end

  @main def main(csvFile: String): Unit =
    Using(Source.fromFile(csvFile)) { _.getLines().foreach(println) }
    CSVParser.parse(Paths.get(csvFile))(userParser) match
      case Left(f) => println(s"ERROR: $f")
      case Right(r) => r.foreach(println)
