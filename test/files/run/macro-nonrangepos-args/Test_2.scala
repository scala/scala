//> using options -Yrangepos:false
object Test extends App {
  val num = 42
  val pos = Macros.pos(num + 17)
  val text = "num + 17"
  assert(pos == s"Line: 4. Width: ${text.length}.", pos) // position of binary op is always a range
}
