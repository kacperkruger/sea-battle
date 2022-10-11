import cats.effect.{IO, IOApp}
import consoles.LiveConsole
import games.GameRuntime

object Main extends IOApp.Simple:
  override val run: IO[Unit] = new GameRuntime(LiveConsole).run.foreverM
