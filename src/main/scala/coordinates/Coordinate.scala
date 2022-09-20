package coordinate

import coordinate.error.{
  CoordinateError,
  FirstCoordinateOutOfRange,
  InvalidLengthOfCoordinates,
  SecondCoordinateOutOfRange
}

final case class CoordinateA(x: Int, y: Int)

object CoordinateA {
  private val Letters = LazyList.from('A').take(10).toVector

  def parse(value: String): Either[CoordinateError, CoordinateA] = {
    def fromLetterCoordinate(c: Char): Either[CoordinateError, Int] =
      Letters.indexOf(c) match {
        case -1 => Left(FirstCoordinateOutOfRange)
        case x  => Right(x)
      }

    def fromDigitCoordinate(c: Char): OptionT[CoordinateError, Int] = {
      if (c.isDigit && c.asDigit - 1 >= 0 && c.asDigit - 1 < 10)
        Right(c.asDigit - 1)
      else Left(SecondCoordinateOutOfRange)
    }

    def divideCoordinates(s: String): Either[CoordinateError, (Char, Char)] = {
      if (s.length == 2) Right((s.head, s.last))
      else Left(InvalidLengthOfCoordinates)
    }

  }
}

object Playground extends App {
  println("working")
}
