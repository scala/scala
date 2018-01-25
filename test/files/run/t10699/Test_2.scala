object Test extends App {
  val a_1 = new A_1
  val t     = "t"
  val other = "other"
  assert(a_1.identity_inst(other = other, t = t) == t)
  assert(A_1.identity_static(other = other, t = t) == t)
}