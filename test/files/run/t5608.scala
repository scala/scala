object Test {
  def main(args:Array[String]) {
    val ns = Array(3L, 3L, 3L)
    val a1: A = new A(ns(0))
    val a2: A = new A(ns(0))
    println(a1 + a2)
  }
}

class A(val u: Long) extends AnyVal {
  def +(other: A) = new A(other.u + u)
}
