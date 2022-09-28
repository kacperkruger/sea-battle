package core

import cats.syntax.show
import cats.{Semigroup, Show}
import core.Ship.{Battleship, Carrier}

trait Ship {
  val size: Int
}
object Ship {
  object Carrier extends Ship {
    override val size: Int = 5
  }
  object Battleship extends Ship {
    override val size: Int = 4
  }
  object Submarine extends Ship {
    override val size: Int = 3
  }
  object Cruiser extends Ship {
    override val size: Int = 3
  }
  object Destroyer extends Ship {
    override val size: Int = 2
  }

  given Show[Ship] with {
    def show(ship: Ship): String = ship match {
      case Battleship => s"Battleship (${Battleship.size})"
      case Carrier    => s"Carrier (${Carrier.size})"
      case Destroyer  => s"Destroyer (${Destroyer.size})"
      case Cruiser    => s"Cruiser (${Cruiser.size})"
      case Submarine  => s"Submarine (${Submarine.size})"
      case _          => "Unknown ship"
    }
  }
}
