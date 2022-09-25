package core

import core.Utilities.times

import scala.annotation.tailrec

case class Board(value: List[List[FieldStatus]])

object Board {
  def create: Board = {
    Board(
      List.fill(BoardSize.size)(
        List.fill(BoardSize.size)(FieldStatus.Empty)
      )
    )
  }

  extension (board: Board) {
    def isCorrectCoordinate(coordinate: Coordinate): Boolean = {
      import coordinate.*
      x < BoardSize.size && x >= 0 && y < BoardSize.size && y >= 0
    }

    def isEmpty(coordinate: Coordinate): Boolean =
      apply(coordinate) == FieldStatus.Empty

    def apply(coordinate: Coordinate): Either[BoardError, FieldStatus] = {
      import coordinate.*
      if (isCorrectCoordinate(coordinate)) {
        Right(board.value(y)(x))
      } else Left(CoordinateOutOfOrder)
    }

    def update(
        coordinate: Coordinate,
        fieldStatus: FieldStatus
    ): Either[BoardError, Board] = {
      import coordinate.*
      if (isCorrectCoordinate(coordinate)) {
        val line = board.value(y)
        Right(Board(board.value.updated(y, line.updated(x, fieldStatus))))
      } else Left(CoordinateOutOfOrder)
    }

    def updateMultiply(
        coordinates: List[Coordinate],
        fieldStatus: FieldStatus
    ): Either[BoardError, Board] =
      coordinates.foldLeft[Either[BoardError, Board]](Right(board))(
        (accEither, coordinate) =>
          for {
            acc <- accEither
            updatedBoard <- acc.update(coordinate, fieldStatus)
          } yield updatedBoard
      )

    @tailrec
    def canPlaceShip(
        coordinate: Coordinate,
        size: Int,
        direction: Direction,
        acc: Boolean = true
    ): Boolean = {
      if (size == 0) acc
      else if (!(isCorrectCoordinate(coordinate) && isEmpty(coordinate))) false
      else
        canPlaceShip(
          Coordinate.move(coordinate, direction),
          size - 1,
          direction,
          true
        )
    }

    def placeShip(
        ship: Ship,
        coordinate: Coordinate,
        direction: Direction
    ): Either[BoardError, Board] = {
      if (
        isCorrectCoordinate(coordinate) && canPlaceShip(
          coordinate,
          ship.size,
          direction
        )
      )
        updateMultiply(
          (1 to ship.size).toList
            .map(n =>
              times(coordinate, Coordinate.move(_: Coordinate, direction), n)
            ),
          FieldStatus.Ship
        )
      else Left(CanNotPlaceShip)
    }
  }
}