package kacperkruger.seabattle.games

import cats.implicits.catsSyntaxEitherId
import cats.syntax.all.*
import kacperkruger.seabattle.boards.Board
import kacperkruger.seabattle.boards.Board.numberOfShips
import kacperkruger.seabattle.boards.coordinates.Coordinate
import kacperkruger.seabattle.boards.directions.domain.Direction
import kacperkruger.seabattle.games.errors.*
import kacperkruger.seabattle.games.phases.{PlacingPhase, PlayingPhase}
import kacperkruger.seabattle.games.status.{
  GameStatus,
  OnGoing,
  WaitingForPlayers,
  Won
}
import kacperkruger.seabattle.players.Player
import kacperkruger.seabattle.ships.Ship

final case class Game(
    board1: Board,
    board2: Board,
    player1: Option[Player],
    player2: Option[Player],
    status: GameStatus,
    moves: Int
)

object Game {
  def create(): Game = {
    Game(Board.create(), Board.create(), None, None, WaitingForPlayers, 0)
  }

  def connect(game: Game, newPlayer: Player): Either[GameError, Game] = {
    if (game.player1.isDefined && game.player2.isDefined) GameIsFull.asLeft
    else if (game.player1.isDefined)
      Game(
        game.board1,
        game.board2,
        game.player1,
        Some(newPlayer),
        OnGoing(game.player1.get, PlacingPhase),
        game.moves
      ).asRight
    else
      Game(
        game.board1,
        game.board2,
        Some(newPlayer),
        game.player2,
        WaitingForPlayers,
        game.moves
      ).asRight
  }

  def update(
      game: Game,
      nextBoard: Board
  ): Either[GameError, Game] = {
    game.status match {
      case WaitingForPlayers => NotEnoughPlayers.asLeft
      case Won(winner)       => GameIsOver.asLeft
      case OnGoing(nextPlayer, phase) =>
        phase match {
          case PlacingPhase =>
            updatePlacingPhase(game, nextPlayer, nextBoard).asRight
          case PlayingPhase =>
            updatePlayingPhase(game, nextPlayer, nextBoard).asRight
        }
    }
  }

  private def updatePlacingPhase(
      game: Game,
      nextPlayer: Player,
      updatedBoard: Board
  ): Game = {
    if (
      Board.numberOfShips(
        updatedBoard
      ) == 17 && game.player2.get == nextPlayer
    )
      Game(
        game.board1,
        updatedBoard,
        game.player1,
        game.player2,
        OnGoing(game.player1.get, PlayingPhase),
        game.moves + 1
      )
    else if (
      Board.numberOfShips(
        updatedBoard
      ) == 17 && game.player1.get == nextPlayer
    )
      Game(
        updatedBoard,
        game.board2,
        game.player1,
        game.player2,
        OnGoing(game.player2.get, PlacingPhase),
        game.moves + 1
      )
    else if (game.player1.get == nextPlayer)
      Game(
        updatedBoard,
        game.board2,
        game.player1,
        game.player2,
        OnGoing(nextPlayer, PlacingPhase),
        game.moves + 1
      )
    else
      Game(
        game.board1,
        updatedBoard,
        game.player1,
        game.player2,
        OnGoing(nextPlayer, PlacingPhase),
        game.moves + 1
      )
  }

  private def updatePlayingPhase(
      game: Game,
      nextPlayer: Player,
      updatedBoard: Board
  ): Game = {
    if (
      Board.numberOfShips(updatedBoard) == 0 && nextPlayer == game.player1.get
    )
      Game(
        game.board1,
        updatedBoard,
        game.player1,
        game.player2,
        Won(nextPlayer),
        game.moves + 1
      )
    else if (
      Board.numberOfShips(updatedBoard) == 0 && nextPlayer == game.player2.get
    )
      Game(
        updatedBoard,
        game.board2,
        game.player1,
        game.player2,
        Won(nextPlayer),
        game.moves + 1
      )
    else if (nextPlayer == game.player1.get)
      Game(
        game.board1,
        updatedBoard,
        game.player1,
        game.player2,
        OnGoing(game.player2.get, PlayingPhase),
        game.moves + 1
      )
    else
      Game(
        updatedBoard,
        game.board2,
        game.player1,
        game.player2,
        OnGoing(game.player1.get, PlayingPhase),
        game.moves + 1
      )
  }

  def findOpponent(game: Game, player: Player): Either[GameError, Board] = {
    if (game.player1.contains(player)) game.board2.asRight
    else if (game.player2.contains(player)) game.board1.asRight
    else CanNotFindPlayer.asLeft
  }

  def findBoard(game: Game, player: Player): Either[GameError, Board] = {
    if (game.player1.isDefined && player == game.player1.get)
      game.board1.asRight
    else if (game.player2.isDefined && player == game.player2.get)
      game.board2.asRight
    else CanNotFindPlayer.asLeft
  }

  def shoot(
      game: Game,
      coordinate: Coordinate,
      player: Player
  ): Either[GameError, Game] = {
    import game.*
    status match {
      case WaitingForPlayers => NotEnoughPlayers.asLeft
      case Won(winner)       => GameIsOver.asLeft
      case OnGoing(nextPlayer, phase) =>
        if (player != nextPlayer) WrongPlayer.asLeft
        else
          phase match {
            case PlacingPhase => CanNotShootDuringPlacingPhase.asLeft
            case PlayingPhase =>
              for {
                opponentBoard <- Game.findOpponent(game, player)
                newBoard <- Board.shoot(
                  opponentBoard,
                  coordinate
                )
                isShipHit = Board.shipHit(opponentBoard, coordinate)
                updatedGame <-
                  if (isShipHit)
                    Game.update(
                      game,
                      newBoard
                    )
                  else
                    Game.update(game, newBoard)
              } yield updatedGame
          }
    }
  }

  def placeShip(
      game: Game,
      coordinate: Coordinate,
      player: Player,
      ship: Ship,
      direction: Direction
  ): Either[GameError, Game] = {
    import game.*
    status match {
      case WaitingForPlayers => NotEnoughPlayers.asLeft
      case Won(winner)       => GameIsOver.asLeft
      case OnGoing(nextPlayer, phase) =>
        if (player != nextPlayer) WrongPlayer.asLeft
        else
          phase match {
            case PlayingPhase => CanNotPlaceDuringPlayPhase.asLeft
            case PlacingPhase =>
              for {
                board <- Game.findBoard(game, player)
                updatedBoard <- Board.placeShip(
                  board,
                  ship,
                  coordinate,
                  direction
                )
                updatedGame <- Game.update(game, updatedBoard)
              } yield updatedGame
          }
    }
  }
}
