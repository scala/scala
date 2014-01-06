object Test {

  sealed abstract class Expr
  // Change type of `arg` to `Any` and the exhaustiveness warning
  // is issued below
  case class Op(arg: Expr) extends Expr
  case class NotHandled(num: Double) extends Expr

  def exhausto(expr: Expr): Unit = expr match {
    case Op(Op(_)) =>
    case Op(_) =>
  }
}
