package consoles

import cats.effect.IO

trait Console:
  def readLine: IO[String]
  def printLine(s: String): IO[Unit]
