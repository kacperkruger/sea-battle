package kacperkruger.seabattle

import cats.effect.{IO, IOApp}
import kacperkruger.seabattle.consoles.LiveConsole
import kacperkruger.seabattle.games.GameRuntime

object Main extends IOApp.Simple:
  override val run: IO[Unit] = new GameRuntime(LiveConsole).run.foreverM
