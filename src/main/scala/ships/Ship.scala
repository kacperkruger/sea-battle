package ships

import cats.syntax.show
import cats.{Semigroup, Show}
import ships.Ship

trait Ship {
  val name: String
  val size: Int
}
object Ship {

  given Show[Ship] with {
    def show(ship: Ship): String = s"${ship.name} (${ship.size})"
  }
}
