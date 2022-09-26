package core

trait GameStatus

case class OnGoing(nextPlayer: Player) extends GameStatus
case class Won(winner: Player) extends GameStatus
