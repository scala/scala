case class C(x: Int = 1)
object C {
  def apply(x: Int = 2) = new C(x)
}
object Test extends App {
  assert(new C().x == 1)
  assert(C().x == 2)
}
