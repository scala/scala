//> using options -Xsource:3
//

trait X
trait Y

class Test {
  val a: Map[Int, X] & Map[Int, Y] = Map[Int, X & Y]() // ok
  val b: Int Map X & Int Map Y = Map[Int, X & Y]() // error: unsupported

  // This one is unambiguous but it's hard to check whether parens were present
  // from the parser output so we also emit an error there.
  val c: (Int Map X) & (Int Map Y) = Map[Int, X & Y]() // error: unsupported
}
