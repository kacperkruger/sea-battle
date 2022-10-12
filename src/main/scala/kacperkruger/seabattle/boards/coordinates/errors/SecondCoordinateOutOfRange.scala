package kacperkruger.seabattle.boards.coordinates.errors

object SecondCoordinateOutOfRange extends CoordinateError:
  override def errorMessage: String = "First coordinate out of range"
