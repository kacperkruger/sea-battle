package core

trait GameError {
  def errorMessage: String
}

object WrongPlayer extends GameError {
  override def errorMessage: String = "Wrong player"
}

object GameIsOver extends GameError {
  override def errorMessage: String = "Game has already ended"
}

object CanNotPlaceDuringPlayPhase extends GameError {
  override def errorMessage: String = "Can't place a ship during playing phase"
}

object CanNotShootDuringPlacingPhase extends GameError {
  override def errorMessage: String = "Can't shoot during placing phase"
}

object CanNotFindPlayer extends GameError {
  override def errorMessage: String = "Can't find the player"
}
