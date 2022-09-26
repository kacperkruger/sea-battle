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
