package coordinates

import board.BoardSize
import coordinates.error.{
  CoordinateError,
  FirstCoordinateMustBeLetter,
  FirstCoordinateOutOfRange,
  InvalidLengthOfCoordinates,
  SecondCoordinateMustBeDigit,
  SecondCoordinateOutOfRange
}

final case class Coordinate(x: Int, y: Int)

object Coordinate {
  private val Letters = LazyList.from('A').take(BoardSize.size).toVector

  def parse(value: String): Either[CoordinateError, Coordinate] = {
    def fromLetterCoordinate(c: Char): Either[CoordinateError, Int] =
      if (!c.isLetter) Left(FirstCoordinateMustBeLetter)
      else if (!Letters.contains(c)) Left(FirstCoordinateOutOfRange)
      else Right(Letters.indexOf(c))

    def fromDigitCoordinate(c: Char): Either[CoordinateError, Int] = {
      if (!c.isDigit) Left(SecondCoordinateMustBeDigit)
      else if (c.asDigit - 1 >= 0 && c.asDigit - 1 < BoardSize.size)
        Right(c.asDigit - 1)
      else Left(SecondCoordinateOutOfRange)
    }

    def divideCoordinates(s: String): Either[CoordinateError, (Char, Char)] = {
      if (s.length == 2) Right((s.head, s.last))
      else Left(InvalidLengthOfCoordinates)
    }

    for {
      charCoordinates <- divideCoordinates(value)
      letterCoordinate <- fromLetterCoordinate(charCoordinates._1)
      digitCoordinate <- fromDigitCoordinate(charCoordinates._2)
    } yield Coordinate(letterCoordinate, digitCoordinate)
  }

  extension (value: String) {
    def parseCoordinates: Either[CoordinateError, Coordinate] =
      Coordinate.parse(value)
  }
}
