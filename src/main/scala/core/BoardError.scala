package core

sealed trait BoardError {
  def errorMessages: String
}

object CoordinateOutOfOrder extends BoardError {
  override def errorMessages: String = "Yours coordinates aim out of board"
}

object CanNotPlaceShip extends BoardError {
  override def errorMessages: String = "It is impossible to place sip there"
}

object CanNotShootToShotField extends BoardError {
  override def errorMessages: String = "You shot there before"
}
