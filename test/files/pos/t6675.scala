object LeftOrRight {
  def unapply[A](value: Either[A, A]): Option[A] = value match {
    case scala.Left(x) => Some(x)
    case scala.Right(x) => Some(x)
  }
}

object Test {
  (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match {
    case LeftOrRight(pair @ (a, b)) => a // false -Xlint warning: "extractor pattern binds a single value to a Product2 of type (Int, Int)"
  }

  (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match {
    case LeftOrRight((a, b)) => a // false -Xlint warning: "extractor pattern binds a single value to a Product2 of type (Int, Int)"
  }

  (Left((0, 0)): Either[(Int, Int), (Int, Int)]) match {
    case LeftOrRight(a, b) => a // false -Xlint warning: "extractor pattern binds a single value to a Product2 of type (Int, Int)"
  }
}
