package kacperkruger.seabattle.games.errors

object CanNotShootDuringPlacingPhase extends GameError:
  override def errorMessage: String = "Can't shoot during placing phase"
