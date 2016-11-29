class C {
  val z = 2
  def mod = { object x { val y = z } ; x.y }
}

object Test extends App {
  val c = new C
  assert(c.mod == c.z, s"${c.mod} != ${c.z}")
}
