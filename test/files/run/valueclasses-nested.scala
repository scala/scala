case class Val[T](val x: T) extends AnyVal {
  override def toString = s"Val($x)"
  def pair = new Val((x, x))
}

object Val {
  def box[T](x: T) = new Val(x)
}

object Test {
  def decompose(x: Any) = x match {
    case Val(Val(Val(y))) => s"is a val^3 of $y"
    case Val(Val(y)) => s"is a val^2 of $y"
    case Val(y) => s"is a val of $y"
    case _ => s"is not a val"
  }
  def printDecompose(xs: Any*) = for (x <- xs) println(s"$x${decompose(x)}")

  def main(args: Array[String]) {
    val v: Val[Int] = Val.box(1)
    val vv = new Val(v)
    val vvv = new Val(vv)
    val vvp = new Val(v.pair)
    println(s"$v $vv $vvv $vvp")
    printDecompose(v, vv, vvv, vvp)
  }
}

// From SI-6045
class A(val i: Int) extends AnyVal
class B(val a: A) extends AnyVal
