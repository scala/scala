// constructors used to drop outer fields when they were not accessed
// however, how can you know (respecting separate compilation) that they're not accessed!?
class Outer { final class Inner }

// the matches below require Inner's outer pointer
object Test extends App {
  val a = new Outer
  val b = new Outer
  (new a.Inner: Any) match {
    case _: b.Inner => println("b")
    case _: a.Inner => println("a")
  }
  (new b.Inner: Any) match {
    case _: a.Inner => println("a")
    case _: b.Inner => println("b")
  }
}
