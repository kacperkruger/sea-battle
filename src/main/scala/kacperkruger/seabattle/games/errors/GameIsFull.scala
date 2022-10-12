package kacperkruger.seabattle.games.errors

object GameIsFull extends GameError:
  override def errorMessage: String =
    "This game is fully. Try to join to another game"
