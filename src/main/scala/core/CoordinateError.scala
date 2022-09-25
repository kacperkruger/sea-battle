package core

sealed trait CoordinateError {
  def errorMessages: String
}

object FirstCoordinateOutOfRange extends CoordinateError {
  override def errorMessages: String = "First coordinate out of range"
}

object FirstCoordinateMustBeLetter extends CoordinateError {
  override def errorMessages: String = "First coordinate must be letter"
}

object SecondCoordinateOutOfRange extends CoordinateError {
  override def errorMessages: String = "First coordinate out of range"
}

object SecondCoordinateMustBeDigit extends CoordinateError {
  override def errorMessages: String = "Second coordinate must be digit"
}

object InvalidLengthOfCoordinates extends CoordinateError {
  override def errorMessages: String =
    "Yours coordinates are too long or too short"
}
