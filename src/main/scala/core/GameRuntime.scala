package core

import cats.Show.Shown
import cats.data.Validated
import cats.effect.IO
import cats.syntax.all.*
import core.Ship.{Battleship, Carrier, Cruiser, Destroyer, Submarine}

final class GameRuntime(console: Console):
  private def readCoordinateLoop(nextPlayer: Player): IO[Coordinate] =
    for {
      _ <- console.printLine(show"Player ${nextPlayer} make a move: ")
      line <- console.readLine
      result <- Coordinate.parseCoordinate(line) match {
        case Validated.Valid(result) => result.pure[IO]
        case Validated.Invalid(errors) =>
          console.printLine(
            errors.toChain.toList.map(_.errorMessage).mkString("\n")
          ) *> readCoordinateLoop(nextPlayer)
      }
    } yield result

  def printPlayerBoard(game: Game, player: Player): IO[Unit] = {
    for {
      _ <- Game.findBoard(game, player) match {
        case Right(playerBoard) => console.printLine(show"\n$playerBoard\n")
        case Left(error)        => console.printLine(error.errorMessage)
      }
    } yield ()
  }

  def printOpponentBoard(game: Game, player: Player): IO[Unit] = {
    import core.ShowBoardWithoutShips.given
    for {
      _ <- Game.findOpponent(game, player) match {
        case Right(opponentBoard) => console.printLine(show"\n$opponentBoard\n")
        case Left(error)          => console.printLine(error.errorMessage)
      }
    } yield ()
  }

  private def readDirectionLoop(
      nextPlayer: Player
  ): IO[Direction] = {
    for {
      _ <- console.printLine(
        show"$nextPlayer pass the direction to make a move:"
      )
      line <- console.readLine
      result <- Direction.parse(line) match {
        case Right(result) => result.pure[IO]
        case Left(error) =>
          console.printLine(error.errorMessage) *> readDirectionLoop(
            nextPlayer
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
          _ <- console.printLine(show"\n$nextPlayer place the $ship\n")
          coordinate <- readCoordinateLoop(nextPlayer)
          direction <- readDirectionLoop(nextPlayer)
          updatedGame <- Game.placeShip(
            game,
            coordinate,
            nextPlayer,
            ship,
            direction
          ) match {
            case Right(result) =>
              result.pure[IO]
            case Left(error) =>
              console.printLine(error.errorMessage) *> placingShipLoop(
                game,
                nextPlayer,
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

  def readPlayerLoop(): IO[Player] = for {
    _ <- console.printLine("Type player name: ")
    name1 <- console.readLine
  } yield Player(name1)

  def connectPlayerLoop(game: Game): IO[Game] = for {
    player <- readPlayerLoop()
    connectedPlayerGame <- Game.connect(game, player) match {
      case Right(updatedGame) => updatedGame.pure[IO]
      case Left(error) =>
        console.printLine(error.errorMessage) *> connectPlayerLoop(
          game
        )
    }
  } yield connectedPlayerGame

  private def loop(game: Game): IO[Unit] = for {
    _ <- game.status match {
      case WaitingForPlayers => console.printLine("Waiting for more players...")
      case Won(winner) => console.printLine(show"\n $winner won the game!\n")
      case OnGoing(nextPlayer, phase) =>
        phase match {
          case PlacingPhase =>
            for {
              updatedGame <- placingPhaseLoop(game, nextPlayer)
              _ <- loop(updatedGame)
            } yield ()
          case PlayingPhase =>
            for {
              _ <- printPlayerBoard(game, nextPlayer)
              _ <- printOpponentBoard(game, nextPlayer)
              move <- readCoordinateLoop(nextPlayer)

              _ <- Game.shoot(
                game,
                move,
                nextPlayer
              ) match {
                case Right(result) => loop(result)
                case Left(error) =>
                  console.printLine(error.errorMessage) *> loop(game)
              }
            } yield ()
        }
    }
  } yield ()

  val run: IO[Unit] =
    val game = Game.create()
    for {
      connectedPlayer1Game <- connectPlayerLoop(game)
      connectedPlayer2Game <- connectPlayerLoop(connectedPlayer1Game)
      _ = println(connectedPlayer2Game)
      _ <- console.printLine("\n-- Starting a new game --\n") >> loop(
        connectedPlayer2Game
      )
    } yield ()
