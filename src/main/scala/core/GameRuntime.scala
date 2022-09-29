package core

import cats.Show.Shown
import cats.data.Validated
import cats.effect.IO
import cats.syntax.all.*
import core.Ship.{Battleship, Carrier, Cruiser, Destroyer, Submarine}

final class GameRuntime(console: Console):
  private def readCoordinateLoop(nextBoard: Board): IO[Coordinate] =
    for {
      _ <- console.printLine(show"Player ${nextBoard.player} make a move: ")
      line <- console.readLine
      result <- Coordinate.parseCoordinate(line) match {
        case Validated.Valid(result) => result.pure[IO]
        case Validated.Invalid(errors) =>
          console.printLine(
            errors.toChain.toList.map(_.errorMessage).mkString("\n")
          ) *> readCoordinateLoop(nextBoard)
      }
    } yield result

//  def printPlayersBoard(
//      player: Player,
//      board1: Board,
//      board2: Board
//  ): IO[Unit] = for {
//    _ <- console.printLine(show"\n$player board\n")
//    _ <- {
//      player match
//        case Player1 => console.printLine(show"\n$board1\n")
//        case Player2 => console.printLine(show"\n$board2\n")
//
//    }
//  } yield ()

  def printOpponentBoard(game: Game, board: Board): IO[Unit] = {
    import core.ShowBoardWithoutShips.given
    for {
      _ <- Game.opponent(game, board) match {
        case Right(value) => console.printLine(show"\n$value\n")
        case Left(error)  => console.printLine(error.errorMessage)
      }
    } yield ()
  }

  private def readDirectionLoop(
      nextBoard: Board
  ): IO[Direction] = {
    for {
      _ <- console.printLine(
        show"${nextBoard.player} pass the direction to make a move:"
      )
      line <- console.readLine
      result <- Direction.parse(line) match {
        case Right(result) => result.pure[IO]
        case Left(error) =>
          console.printLine(error.errorMessage) *> readDirectionLoop(
            nextBoard
          )
      }
    } yield result
  }

  private def placingShipLoop(
      game: Game,
      nextPlayer: Player,
      ship: Ship
  ): IO[Game] =
    Game.findBoard(game, nextPlayer) match {
      case Left(error) => console.printLine(error.errorMessage) *> game.pure[IO]
      case Right(board) =>
        for {
          _ <- console.printLine(show"$board")
          _ <- console.printLine(show"\n${board.player} place the $ship\n")
          coordinate <- readCoordinateLoop(board)
          direction <- readDirectionLoop(board)
          updatedGame <- Game.placeShip(
            game,
            coordinate,
            board,
            ship,
            direction
          ) match {
            case Right(result) =>
              result.pure[IO]
            case Left(error) =>
              console.printLine(error.errorMessage) *> placingShipLoop(
                game,
                board.player,
                ship
              )
          }
        } yield updatedGame
    }

  private def placingPhaseLoop(
      game: Game,
      nextPlayer: Player
  ): IO[Game] = for {
    placedCarrier <- placingShipLoop(game, nextPlayer, Carrier)
    placedBattleship <- placingShipLoop(placedCarrier, nextPlayer, Battleship)
    placedCruiser <- placingShipLoop(placedBattleship, nextPlayer, Cruiser)
    placedSubmarine <- placingShipLoop(placedCruiser, nextPlayer, Submarine)
    placedAll <- placingShipLoop(placedSubmarine, nextPlayer, Destroyer)
  } yield placedAll

  private def loop(game: Game): IO[Unit] = for {
    _ <- game.status match {
      case Won(winner) => console.printLine(show"\n $winner won the game!\n")
      case OnGoing(nextBoard, phase) =>
        phase match {
          case PlacingPhase =>
            for {
              updatedGame <- placingPhaseLoop(game, nextBoard.player)
              _ <- loop(updatedGame)
            } yield ()
          case PlayingPhase =>
            for {
              _ <- console.printLine("")
              _ <- console.printLine(show"$nextBoard")
              _ <- console.printLine("")
              _ <- printOpponentBoard(game, nextBoard)
              move <- readCoordinateLoop(nextBoard)

              _ <- Game.shoot(
                game,
                move,
                nextBoard
              ) match {
                case Right(result) => loop(result)
                case Left(error) =>
                  console.printLine(error.errorMessage) *> loop(game)
              }
            } yield ()
        }
    }
  } yield ()

  val run: IO[Unit] = for {
    _ <- console.printLine("Type first player name: ")
    name1 <- console.readLine
    _ <- console.printLine("Type second player name: ")
    name2 <- console.readLine
    _ <- console.printLine("\n-- Starting a new game --\n") >> loop(
      Game.create(Player(name1), Player(name2))
    )
  } yield ()
