package kacperkruger.seabattle.players

import cats.Show
import cats.kernel.Eq

case class Player(name: String)

object Player {
  def create(name: String): Player = Player(name)

  given Show[Player] with {
    def show(player: Player): String = player.name
  }
  given Eq[Player] = Eq.fromUniversalEquals
}
