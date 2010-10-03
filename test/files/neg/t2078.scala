class A[-S](y : S) {
  val f  = new { val x = y }
}

object Test extends Application {
  val a = new A(1)
  val b = a : A[Nothing]
  b.f.x
}
