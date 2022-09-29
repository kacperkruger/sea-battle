package core

import cats.implicits.catsSyntaxEitherId
import cats.syntax.all._

final case class Game(
    board1: Board,
    board2: Board,
    status: GameStatus,
    moves: Int
)

object Game {
  def create(player1: Player, player2: Player): Game =
    val board1 = Board.create(player1)
    Game(board1, Board.create(player2), OnGoing(board1, PlacingPhase), 0)

  def update(
      game: Game,
      board: Board,
      status: GameStatus
  ): Game = {
    if (board.player == game.board1.player)
      Game(board, game.board2, status, game.moves + 1)
    else Game(game.board1, board, status, game.moves + 1)
  }

  def opponent(game: Game, board: Board): Either[GameError, Board] = {
    if (board == game.board1) game.board2.asRight
    else if (board == game.board2) game.board1.asRight
    else CanNotFindPlayer.asLeft
  }

  def findBoard(game: Game, player: Player): Either[GameError, Board] = {
    if (player == game.board1.player) game.board1.asRight
    else if (player == game.board2.player) game.board2.asRight
    else CanNotFindPlayer.asLeft
  }

  def shoot(
      game: Game,
      coordinate: Coordinate,
      shootingBoard: Board
  ): Either[GameError, Game] = {
    import game._
    status match {
      case Won(winner) => GameIsOver.asLeft
      case OnGoing(nextBoard, phase) =>
        if (shootingBoard.player != nextBoard.player) WrongPlayer.asLeft
        else
          phase match {
            case PlacingPhase => CanNotShootDuringPlacingPhase.asLeft
            case PlayingPhase =>
              for {
                opponent <- Game.opponent(game, shootingBoard)
                newBoard <- Board.shoot(
                  opponent,
                  coordinate
                )
                isShipHit = Board.shipHit(opponent, coordinate)
              } yield {
                if (isShipHit)
                  Game.update(
                    game,
                    newBoard,
                    OnGoing(shootingBoard, PlayingPhase)
                  )
                else
                  Game.update(
                    game,
                    newBoard,
                    OnGoing(newBoard, PlayingPhase)
                  )
              }
          }
    }
  }

  def placeShip(
      game: Game,
      coordinate: Coordinate,
      board: Board,
      ship: Ship,
      direction: Direction
  ): Either[GameError, Game] = {
    import game._
    status match {
      case Won(winner) => GameIsOver.asLeft
      case OnGoing(nextBoard, phase) =>
        if (board.player != nextBoard.player) WrongPlayer.asLeft
        else
          phase match {
            case PlacingPhase =>
              for {
                opponentBoard <- Game.opponent(game, board)
                newBoard <- Board.placeShip(
                  board,
                  ship,
                  coordinate,
                  direction
                )
              } yield {
                if (Board.numberOfShips(newBoard) == 17)
                  Game.update(
                    game,
                    newBoard,
                    OnGoing(
                      opponentBoard,
                      if (opponentBoard.player == game.board1.player)
                        PlayingPhase
                      else PlacingPhase
                    )
                  )
                else
                  Game.update(
                    game,
                    newBoard,
                    OnGoing(newBoard, PlacingPhase)
                  )
              }
            case PlayingPhase => CanNotPlaceDuringPlayPhase.asLeft
          }
    }
  }
}
