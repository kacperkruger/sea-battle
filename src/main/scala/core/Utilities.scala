package core

import scala.annotation.tailrec

object Utilities {
  @tailrec
  def times[A](value: A, action: A => A, n: Int): A = {
    if (n == 0) value
    else times(action(value), action, n - 1)
  }
}
