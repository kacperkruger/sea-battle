package boards.errors

object CanNotShootToShotField extends BoardError:
  override def errorMessage: String = "You shot there before"
