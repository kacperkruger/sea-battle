package player.domain

import cats.Show

trait Player

case object Player1 extends Player
case object Player2 extends Player

object Player {
  given Show[Player] = Show.fromToString
  given Eq[Player] =
}
