package kacperkruger.seabattle.games.errors

object CanNotPlaceDuringPlayPhase extends GameError:
  override def errorMessage: String = "Can't place a ship during playing phase"
