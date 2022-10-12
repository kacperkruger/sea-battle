package kacperkruger.seabattle.boards.coordinates.errors

object FirstCoordinateOutOfRange extends CoordinateError:
  override def errorMessage: String = "First coordinate out of range"
