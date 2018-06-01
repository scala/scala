class A(a: Int, val b: Int = a)

object Test extends App {
  val a = new A(1)
  val a2 = new A(1, 1)
  assert(a.b == a2.b)
}
