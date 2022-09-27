package core

import cats.implicits.catsSyntaxEitherId

final case class Game(
    board1: Board,
    board2: Board,
    status: GameStatus,
    moves: Int
)

object Game {
  def create: Game = Game(Board.create, Board.create, OnGoing(Player1), 0)

  extension (game: Game) {
    def shoot(
        coordinate: Coordinate,
        player: Player
    ): Either[GameError, Game] = {
      import game._
      status match {
        case Won(winner) => GameIsOver.asLeft
        case OnGoing(nextPlayer) =>
          if (player != nextPlayer) WrongPlayer.asLeft
          else {
            player match {
              case Player1 =>
                val isShipHit = board2.shipHit(coordinate)
                for {
                  newBoard2 <- board2.shoot(coordinate)
                } yield {
                  if (isShipHit)
                    Game(board1, newBoard2, OnGoing(player), moves + 1)
                  else Game(board1, newBoard2, OnGoing(Player2), moves + 1)
                }
              case Player2 =>
                val isShipHit = board1.shipHit(coordinate)
                for {
                  newBoard1 <- board1.shoot(coordinate)
                } yield {
                  if (isShipHit)
                    Game(newBoard1, board2, OnGoing(player), moves + 1)
                  else Game(newBoard1, board2, OnGoing(Player1), moves + 1)
                }
            }
          }
      }

    }
    def placeShip(
        coordinate: Coordinate,
        player: Player,
        ship: Ship,
        direction: Direction
    ): Either[GameError, Game] = {
      import game._
      status match {
        case Won(winner) => GameIsOver.asLeft
        case OnGoing(nextPlayer) =>
          if (player != nextPlayer) WrongPlayer.asLeft
          player match {
            case Player1 =>
              for {
                newBoard1 <- board1.placeShip(ship, coordinate, direction)
              } yield Game(newBoard1, board2, OnGoing(Player2), moves + 1)
            case Player2 =>
              for {
                newBoard2 <- board2.placeShip(ship, coordinate, direction)
              } yield Game(board1, newBoard2, OnGoing(Player1), moves + 1)
          }
      }
    }
  }
}
