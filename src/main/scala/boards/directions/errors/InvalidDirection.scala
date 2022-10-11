package boards.directions.errors

object InvalidDirection extends DirectionError:
  override def errorMessage: String = "Invalid direction"
