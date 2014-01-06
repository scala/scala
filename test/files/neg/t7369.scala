object Test {
  val X, Y = true
  (null: Tuple1[Boolean]) match {
    case Tuple1(X) =>
    case Tuple1(Y) =>
    case Tuple1(X) => // unreachable
    case _      =>
  }

  (null: Tuple1[Boolean]) match {
    case Tuple1(true) =>
    case Tuple1(false) =>
    case Tuple1(true) => // unreachable
    case _      =>
  }
}


sealed abstract class B;
case object True extends B;
case object False extends B;

object Test2 {

  val X: B = True
  val Y: B = False

  (null: Tuple1[B]) match {
    case Tuple1(X) =>
    case Tuple1(Y) =>
    case Tuple1(X) => // unreachable
    case _      =>
  }
}

object Test3 {
  (null: Tuple1[B]) match {
    case Tuple1(null) =>
    case Tuple1(True) =>
    case Tuple1(null) => // unreachable
    case _      =>
  }
}
