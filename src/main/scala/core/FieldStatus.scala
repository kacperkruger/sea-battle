package core

trait FieldStatus

object FieldStatus {
  object Ship extends FieldStatus
  object Empty extends FieldStatus
  object DestroyedShip extends FieldStatus
  object MissedShot extends FieldStatus
}
