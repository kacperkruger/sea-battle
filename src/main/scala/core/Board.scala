package core

import cats.Show
import cats.syntax.all._
import core.Utilities.times

import scala.annotation.tailrec

case class Board(value: List[List[FieldStatus]])

object Board {
  def create(): Board = {
    Board(
      List.fill(BoardSize.size)(
        List.fill(BoardSize.size)(FieldStatus.Empty)
      )
    )
  }

  def apply(
      board: Board,
      coordinate: Coordinate
  ): Either[BoardError, FieldStatus] = {
    import coordinate.*
    if (isCorrectCoordinate(coordinate)) {
      Right(board.value(y)(x))
    } else Left(CoordinateOutOfOrder)
  }

  def isCorrectCoordinate(coordinate: Coordinate): Boolean = {
    import coordinate.*
    x < BoardSize.size && x >= 0 && y < BoardSize.size && y >= 0
  }

  def isEmpty(board: Board, coordinate: Coordinate): Boolean =
    Board.apply(board, coordinate) == Right(FieldStatus.Empty)

  def update(
      board: Board,
      coordinate: Coordinate,
      fieldStatus: FieldStatus
  ): Either[BoardError, Board] = {
    import coordinate.*
    if (Board.isCorrectCoordinate(coordinate)) {
      val line = board.value(y)
      Right(
        Board(
          board.value.updated(y, line.updated(x, fieldStatus))
        )
      )
    } else Left(CoordinateOutOfOrder)
  }

  def updateMultiply(
      board: Board,
      coordinates: List[Coordinate],
      fieldStatus: FieldStatus
  ): Either[BoardError, Board] = {
    coordinates.foldLeft[Either[BoardError, Board]](Right(board))(
      (accEither, coordinate) =>
        for {
          acc <- accEither
          updatedBoard <- Board.update(acc, coordinate, fieldStatus)
        } yield updatedBoard
    )
  }

  def numberOfShips(board: Board): Int = {
    board.value.map(_.count(_ == FieldStatus.Ship)).sum
  }

  @tailrec
  def canPlaceShip(
      board: Board,
      coordinate: Coordinate,
      size: Int,
      direction: Direction,
      acc: Boolean = true
  ): Boolean = {
    if (size == 0) acc
    else if (!Board.isEmpty(board, coordinate)) false
    else
      Board.canPlaceShip(
        board,
        Coordinate.move(coordinate, direction),
        size - 1,
        direction
      )
  }

  def placeShip(
      board: Board,
      ship: Ship,
      coordinate: Coordinate,
      direction: Direction
  ): Either[BoardError, Board] = {
    if (
      Board.isCorrectCoordinate(coordinate) && Board.canPlaceShip(
        board,
        coordinate,
        ship.size,
        direction
      )
    )
      Board.updateMultiply(
        board,
        (0 until ship.size).toList
          .map(n =>
            times(coordinate, Coordinate.move(_: Coordinate, direction), n)
          ),
        FieldStatus.Ship
      )
    else Left(CanNotPlaceShip)
  }

  def shipHit(board: Board, coordinate: Coordinate): Boolean = {
    Board.apply(board, coordinate) == FieldStatus.Ship
  }

  def shoot(board: Board, coordinate: Coordinate): Either[BoardError, Board] = {
    if (!Board.isCorrectCoordinate(coordinate)) Left(CoordinateOutOfOrder)
    else if (Board.isEmpty(board, coordinate))
      Board.update(board, coordinate, FieldStatus.MissedShot)
    else if (Board.apply(board, coordinate) == Right(FieldStatus.MissedShot))
      Left(CanNotShootToShotField)
    else Board.update(board, coordinate, FieldStatus.DestroyedShip)
  }

  def showHelper(
      board: Board,
      empty: String,
      ship: String,
      missedShot: String,
      destroyedShip: String
  ): String = {
    val numbers =
      LazyList.from(1).take(board.value.size).map(String.format("%1$2d", _))

    val letters =
      LazyList.from('A').take(board.value.size).map(_.toChar.toString)

    ("   " + letters.mkString(" ") + "\n") + board.value
      .zip(numbers)
      .map { case (line, number) =>
        (number + " ") + line
          .map {
            case FieldStatus.Empty         => empty
            case FieldStatus.Ship          => ship
            case FieldStatus.MissedShot    => missedShot
            case FieldStatus.DestroyedShip => destroyedShip
          }
          .mkString(" ")
      }
      .mkString("\n")

  }

  given Show[Board] with {
    def show(board: Board): String = Board.showHelper(board, " ", "☐", " ", "☒")
  }
}

object ShowBoardWithoutShips {
  given Show[Board] with {
    def show(board: Board): String = Board.showHelper(board, " ", " ", "☉", "☒")
  }
}
