// scalac: -Xsource:2.14
//
// without backticks, "not found: value +"
//
object Test extends App {
  val a = 7
  val x = 1
    + //
    `a` * 6

  assert(x == 1)
}
