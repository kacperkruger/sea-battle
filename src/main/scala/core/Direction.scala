package core
import cats.syntax.all._

trait Direction

object Direction {
  object Left extends Direction
  object Right extends Direction
  object Up extends Direction
  object Down extends Direction

  def parse(s: String): Either[DirectionError, Direction] =
    s match {
      case "L" => Left.asRight
      case "R" => Right.asRight
      case "U" => Up.asRight
      case "D" => Down.asRight
      case _   => InvalidDirection.asLeft
    }
}
