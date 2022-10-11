package games.errors

object GameIsOver extends GameError:
  override def errorMessage: String = "Game has already ended"
