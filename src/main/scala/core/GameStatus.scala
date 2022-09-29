package core

import core.GamePhase

trait GameStatus

case class OnGoing(board: Board, phase: GamePhase) extends GameStatus
case class Won(winner: Player) extends GameStatus
