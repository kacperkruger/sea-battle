package boards.errors

object CanNotPlaceShip extends BoardError:
  override def errorMessage: String = "It is impossible to place sip there"
