object D {
  def main(args: Array[String]) {
    val a = new { def y[T](x: X[T]) = x.i }
    val b = new { def y[T](x: T): X[T] = new X(2) }
    val x = new X(3)
    val t = a.y(x)
    println(t)
  }
}

class X[T](val i: Int) extends AnyVal

