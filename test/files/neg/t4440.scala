// constructors used to drop outer fields when they were not accessed
// however, how can you know (respecting separate compilation) that they're not accessed!?
class Outer { final class Inner }

// the matches below require Inner's outer pointer
// until SI-4440 is fixed properly, we can't make this a run test
// in principle, the output should be "a\nb", but without outer checks it's "b\na"
object Test extends App {
  val a = new Outer
  val b = new Outer
  (new a.Inner: Any) match {
    case _: b.Inner => println("b")
    case _: a.Inner => println("a") // this is the case we want
  }
  (new b.Inner: Any) match {
    case _: a.Inner => println("a")
    case _: b.Inner => println("b") // this is the case we want
  }
}
