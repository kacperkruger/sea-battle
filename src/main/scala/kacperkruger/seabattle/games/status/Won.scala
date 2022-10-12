package kacperkruger.seabattle.games.status

import kacperkruger.seabattle.players.Player

case class Won(winner: Player) extends GameStatus
