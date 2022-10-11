package boards.directions.domain

import boards.directions.errors.{DirectionError, InvalidDirection}
import cats.syntax.all.*

trait Direction

object Direction {

  def parse(s: String): Either[DirectionError, Direction] =
    s match {
      case "L" => Left.asRight
      case "R" => Right.asRight
      case "U" => Up.asRight
      case "D" => Down.asRight
      case _   => InvalidDirection.asLeft
    }
}
