import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*

final case class Coordinate(x: Int, y: Int)

object Coordinate {
  private val Letters = LazyList.from('A').take(BoardSize.size).toVector

  def validate(
      stringCoordinate: String
  ): ValidatedNec[CoordinateError, String] = {
    def firstCoordinateIsLetter(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      if (s.head.isLetter) s.validNec
      else FirstCoordinateMustBeLetter.invalidNec
    }

    def firstCoordinateCorrectRange(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      if (Letters.contains(s.head)) s.validNec
      else FirstCoordinateOutOfRange.invalidNec
    }

    def lengthOfCoordinatesEqualTwo(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      if (s.length == 2) s.validNec
      else InvalidLengthOfCoordinates.invalidNec
    }

    def secondCoordinateIsDigit(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      if (s.tail.head.isDigit) s.validNec
      else SecondCoordinateMustBeDigit.invalidNec
    }

    def secondCoordinateCorrectRange(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      val secondCoordinate = s.tail.head.asDigit - 1
      if (secondCoordinate >= 0 && secondCoordinate <= BoardSize.size)
        s.validNec
      else SecondCoordinateOutOfRange.invalidNec
    }

    def validateFirstCoordinate(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      try {
        firstCoordinateIsLetter(s) match {
          case Validated.Valid(a)   => firstCoordinateCorrectRange(a)
          case Validated.Invalid(e) => Validated.Invalid(e)
        }
      } catch {
        case _: Throwable => FirstCoordinateMustBeLetter.invalidNec
      }

    }

    def validateSecondCoordinate(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      try {
        secondCoordinateIsDigit(s) match {
          case Validated.Valid(a)   => secondCoordinateCorrectRange(a)
          case Validated.Invalid(e) => Validated.Invalid(e)
        }
      } catch {
        case _: Throwable => SecondCoordinateMustBeDigit.invalidNec
      }

    }

    lengthOfCoordinatesEqualTwo(stringCoordinate) *> validateFirstCoordinate(
      stringCoordinate
    ) *> validateSecondCoordinate(stringCoordinate)
  }

  def parse(
      stringCoordinate: String
  ): ValidatedNec[CoordinateError, Coordinate] = {
    def fromLetterCoordinate(s: String): Int =
      Letters.indexOf(s.head)

    def fromDigitCoordinate(s: String): Int =
      s.tail.head.asDigit - 1

    validate(stringCoordinate) andThen (s =>
      Coordinate(fromLetterCoordinate(s), fromDigitCoordinate(s)).validNec
    )
  }

  extension (coordinateString: String) {
    def parseCoordinate: ValidatedNec[CoordinateError, Coordinate] =
      Coordinate.parse(coordinateString)

    def validateCoordinate: ValidatedNec[CoordinateError, String] =
      Coordinate.validate(coordinateString)
  }
}
