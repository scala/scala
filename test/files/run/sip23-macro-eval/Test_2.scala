object Test extends App {
  val res = Macros.eval
  assert(res == 1)
}
