package consoles

import cats.effect.IO

object LiveConsole extends Console:
  override def readLine: IO[String] = IO.readLine

  override def printLine(s: String): IO[Unit] = IO.println(s)
