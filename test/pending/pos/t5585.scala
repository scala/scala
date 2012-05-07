class Result[+A]

case class Success[A](x: A) extends Result[A]

class Apply[A]

object Apply {
  def apply[A](f: Int => Result[A]): Apply[A] = new Apply[A]
}

object TestUnit {
  def goo : Apply[Option[Int]] = Apply { i =>
    val p = i match {
      case 1 => Success(Some(1))
      case _ => Success(None)
    }
  }
}