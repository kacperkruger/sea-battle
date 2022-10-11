package boards.coordinates.errors

object SecondCoordinateMustBeDigit extends CoordinateError:
  override def errorMessage: String = "Second coordinate must be digit"
