package kacperkruger.seabattle.boards.coordinates.errors

object InvalidLengthOfCoordinates extends CoordinateError:
  override def errorMessage: String =
    "Your coordinates are too long or too short"
