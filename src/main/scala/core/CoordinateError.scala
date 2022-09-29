package core

sealed trait CoordinateError extends BoardError

object FirstCoordinateOutOfRange extends CoordinateError:
  override def errorMessage: String = "First coordinate out of range"

object FirstCoordinateMustBeLetter extends CoordinateError:
  override def errorMessage: String = "First coordinate must be letter"

object SecondCoordinateOutOfRange extends CoordinateError:
  override def errorMessage: String = "First coordinate out of range"

object SecondCoordinateMustBeDigit extends CoordinateError:
  override def errorMessage: String = "Second coordinate must be digit"

object InvalidLengthOfCoordinates extends CoordinateError:
  override def errorMessage: String =
    "Yours coordinates are too long or too short"
