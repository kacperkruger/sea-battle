package games.errors

object CanNotFindPlayer extends GameError:
  override def errorMessage: String = "Can't find the player"
