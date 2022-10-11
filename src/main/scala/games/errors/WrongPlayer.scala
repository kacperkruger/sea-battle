package games.errors

object WrongPlayer extends GameError:
  override def errorMessage: String = "Wrong player"
