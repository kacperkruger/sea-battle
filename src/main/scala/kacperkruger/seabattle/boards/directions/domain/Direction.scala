package kacperkruger.seabattle.boards.directions.domain

import cats.syntax.all.*
import kacperkruger.seabattle.boards.directions.errors.{
  DirectionError,
  InvalidDirection
}

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
