object Test {
  val X, Y = true
  (null: Tuple1[Boolean]) match {
    case Tuple1(X) =>
    case Tuple1(Y) => // unreachable
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
    case Tuple1(Y) => // no warning
    case _      =>
  }
}

object Test3 {
  val X, O = true
  def classify(neighbourhood: (Boolean, Boolean, Boolean)): String = {
    neighbourhood match {
      case (X, X, X) => "middle"
      case (X, X, O) => "right"
      case (O, X, X) => "left"
      case _ => throw new IllegalArgumentException("Invalid")
    }
  }
}