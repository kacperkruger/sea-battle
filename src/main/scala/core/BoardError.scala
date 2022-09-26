package core

trait BoardError extends GameError

object CoordinateOutOfOrder extends BoardError {
  override def errorMessage: String = "Yours coordinates aim out of board"
}

object CanNotPlaceShip extends BoardError {
  override def errorMessage: String = "It is impossible to place sip there"
}

object CanNotShootToShotField extends BoardError {
  override def errorMessage: String = "You shot there before"
}
