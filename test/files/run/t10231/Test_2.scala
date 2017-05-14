object Test extends App {
  val a = new A_1
  val i = new a.Inner(i = 99)
  assert(i.x == 99)
}
