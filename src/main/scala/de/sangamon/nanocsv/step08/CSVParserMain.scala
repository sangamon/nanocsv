package de.sangamon.nanocsv.step08

import cats.syntax.all.*
import cats.effect.*
import de.sangamon.nanocsv.shared.*

import java.nio.file.*
import scala.io.*
import scala.util.*

object CSVParseMain extends IOApp:

  import RowParser.*
  import RowParserDerivable.*

  val userParser: RowParser[User] =
    User.apply.curried.deriveRowParser[User] <* end

  override def run(args: List[String]): IO[ExitCode] =
    args match
      case List(csvFile) =>
        CSVParser.parse(Paths.get(csvFile))(userParser).flatMap {
          _.fold(
            e => IO.blocking { println(s"ERROR: $e") } >> ExitCode.Error.pure,
            r => IO.blocking { r.foreach(println) } >> ExitCode.Success.pure
          )
        }
      case _ =>
        IO.blocking { println("Usage: minicsv <csv file path>") } >> ExitCode.Error.pure
