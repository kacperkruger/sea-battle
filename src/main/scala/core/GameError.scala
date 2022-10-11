package core

trait GameError:
  def errorMessage: String

object WrongPlayer extends GameError:
  override def errorMessage: String = "Wrong player"

object GameIsOver extends GameError:
  override def errorMessage: String = "Game has already ended"

object CanNotPlaceDuringPlayPhase extends GameError:
  override def errorMessage: String = "Can't place a ship during playing phase"

object CanNotShootDuringPlacingPhase extends GameError:
  override def errorMessage: String = "Can't shoot during placing phase"

object CanNotFindPlayer extends GameError:
  override def errorMessage: String = "Can't find the player"

object GameIsFull extends GameError:
  override def errorMessage: String =
    "This game is fully. Try to join to another game"

object NotEnoughPlayers extends GameError:
  override def errorMessage: String = "There is not enough players in this game"
