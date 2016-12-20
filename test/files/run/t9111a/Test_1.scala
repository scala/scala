object Test extends App {
  val i = new A_1.Inner()
  assert(i.newT().x == 2)
  assert(A_1.crossCheck() == 2)
}
