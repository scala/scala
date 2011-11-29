class C {
  implicit def ffer(x: Int) = new { def f : Long = 123L }

  val x1: Long = 5.f
}
