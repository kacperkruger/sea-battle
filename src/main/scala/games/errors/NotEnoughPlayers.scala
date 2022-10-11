package games.errors

object NotEnoughPlayers extends GameError:
  override def errorMessage: String = "There is not enough players in this game"
