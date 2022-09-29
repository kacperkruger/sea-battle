package core

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple:
  override val run: IO[Unit] = GameRuntime(LiveConsole).run.foreverM
