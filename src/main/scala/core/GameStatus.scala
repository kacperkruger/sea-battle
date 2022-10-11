package core

import core.GamePhase

trait GameStatus

object WaitingForPlayers extends GameStatus
case class OnGoing(nextPlayer: Player, phase: GamePhase) extends GameStatus
case class Won(winner: Player) extends GameStatus
