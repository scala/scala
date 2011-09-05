class A[-S](y : S) {
  val f  = new { val x = y }
}

object Test extends App {
  val a = new A(1)
  val b = a : A[Nothing]
  println(b.f.x)
}
