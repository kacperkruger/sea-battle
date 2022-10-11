package games.status

import games.phases.GamePhase
import players.Player

case class OnGoing(nextPlayer: Player, phase: GamePhase) extends GameStatus
