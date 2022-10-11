package boards.coordinates.errors

object FirstCoordinateMustBeLetter extends CoordinateError:
  override def errorMessage: String = "First coordinate must be letter"
