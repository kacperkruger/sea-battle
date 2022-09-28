package core

trait DirectionError extends GameError

object InvalidDirection extends DirectionError:
  override def errorMessage: String = "Invalid direction"
