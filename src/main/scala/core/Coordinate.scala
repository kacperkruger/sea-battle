package core

import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*
import core.Direction
import core.Direction.{Up, Down, Left, Right}

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

    def lengthOfCoordinatesEqualTwoOr3(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      if (s.length == 2 || s.length == 3) s.validNec
      else InvalidLengthOfCoordinates.invalidNec
    }

    def secondCoordinateIsDigit(
        s: String
    ): ValidatedNec[CoordinateError, String] =
      s.tail.toIntOption match {
        case Some(_) => s.validNec
        case None    => SecondCoordinateMustBeDigit.invalidNec
      }

    def secondCoordinateCorrectRange(
        s: String
    ): ValidatedNec[CoordinateError, String] = {
      val secondCoordinate = s.tail.toInt - 1
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

    lengthOfCoordinatesEqualTwoOr3(stringCoordinate) *> validateFirstCoordinate(
      stringCoordinate
    ) *> validateSecondCoordinate(stringCoordinate)
  }

  def parse(
      stringCoordinate: String
  ): ValidatedNec[CoordinateError, Coordinate] = {
    def fromLetterCoordinate(s: String): Int =
      Letters.indexOf(s.head)

    def fromDigitCoordinate(s: String): Int =
      s.tail.toInt - 1

    validate(stringCoordinate) andThen (s =>
      Coordinate(fromLetterCoordinate(s), fromDigitCoordinate(s)).validNec
    )
  }

  def move(coordinate: Coordinate, direction: Direction): Coordinate = {
    import coordinate._
    direction match {
      case Direction.Down  => Coordinate(x, y + 1)
      case Direction.Left  => Coordinate(x - 1, y)
      case Direction.Right => Coordinate(x + 1, y)
      case Direction.Up    => Coordinate(x, y - 1)
    }
  }

  extension (coordinateString: String) {
    def parseCoordinate: ValidatedNec[CoordinateError, Coordinate] =
      Coordinate.parse(coordinateString)

    def validateCoordinate: ValidatedNec[CoordinateError, String] =
      Coordinate.validate(coordinateString)
  }
}
