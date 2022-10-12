package kacperkruger.seabattle.boards

import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import kacperkruger.seabattle.boards.coordinates.Coordinate.parseCoordinate
import kacperkruger.seabattle.boards.coordinates.Coordinate
import kacperkruger.seabattle.boards.coordinates.errors.{
  FirstCoordinateMustBeLetter,
  FirstCoordinateOutOfRange,
  InvalidLengthOfCoordinates,
  SecondCoordinateMustBeDigit,
  SecondCoordinateOutOfRange
}

class CoordinatesSpec extends munit.FunSuite {
  test("should parse correct coordinates") {
    val cases = List(
      ("A1", Valid(Coordinate(0, 0))),
      ("A3", Valid(Coordinate(0, 2))),
      ("B1", Valid(Coordinate(1, 0))),
      ("B2", Valid(Coordinate(1, 1))),
      ("B3", Valid(Coordinate(1, 2))),
      ("C1", Valid(Coordinate(2, 0))),
      ("C3", Valid(Coordinate(2, 2)))
    )

    cases.foreach { case (string, expected) =>
      assert(clue(string.parseCoordinate) == clue(expected))
    }
  }

  test("should fail when first coordinate is not letter") {
    assert(
      clue("11".parseCoordinate) == clue(
        Invalid(Chain(FirstCoordinateMustBeLetter))
      )
    )
  }

  test("should fail when first coordinate is out of range") {
    assert(
      clue("Z1".parseCoordinate) == clue(
        Invalid(Chain(FirstCoordinateOutOfRange))
      )
    )
  }

  test("should fail when second coordinate is not digit") {
    assert(
      clue("AZ".parseCoordinate) == clue(
        Invalid(Chain(SecondCoordinateMustBeDigit))
      )
    )
  }

  test("should fail when second coordinate is out of range") {
    assert(
      clue("A0".parseCoordinate) == clue(
        Invalid(Chain(SecondCoordinateOutOfRange))
      )
    )
  }

  test("should fail when coordinate is shorter than 2") {
    assert(
      clue("A".parseCoordinate) == clue(
        Invalid(Chain(InvalidLengthOfCoordinates, SecondCoordinateMustBeDigit))
      )
    )
  }

  test("should fail when coordinate is longer than 2") {
    assert(
      clue("A4B".parseCoordinate) == clue(
        Invalid(Chain(InvalidLengthOfCoordinates))
      )
    )
  }

  test("should fail multiple reasons") {
    assert(
      clue("1ZB".parseCoordinate) == clue(
        Invalid(
          Chain(
            InvalidLengthOfCoordinates,
            FirstCoordinateMustBeLetter,
            SecondCoordinateMustBeDigit
          )
        )
      )
    )
  }
}
