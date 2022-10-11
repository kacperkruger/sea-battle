package boards.errors

object CoordinateOutOfOrder extends BoardError:
  override def errorMessage: String = "Yours coordinates aim out of board"
