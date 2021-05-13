// scalac: -Xsource:3
//
// was: without backticks, "not found: value +" (but parsed here as +a * 6, where backticks fool the lexer)
// now: + is taken as "solo" infix op
//
object Test extends App {
  val a = 7
  val x = 1
    +
    `a`
    *
    6

  assert(x == 1 + 7 * 6, x)  // was: 1, now: successor(42)
}
