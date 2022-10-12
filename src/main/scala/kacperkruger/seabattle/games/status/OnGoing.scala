package kacperkruger.seabattle.games.status

import kacperkruger.seabattle.games.phases.GamePhase
import kacperkruger.seabattle.players.Player

case class OnGoing(nextPlayer: Player, phase: GamePhase) extends GameStatus
