package kacperkruger.seabattle.games.errors

object NotEnoughPlayers extends GameError:
  override def errorMessage: String =
    "There is not enough kacperkruger.seabattle.players in this game"
