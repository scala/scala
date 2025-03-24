object Test extends App {
  val x = 2
  val pos = Macros.pos(x + 2 + "42".toString)
  val text = """x + 2 + "42".toString"""
  assert(pos == s"Line: 3. Width: ${text.length}.", pos)
}
