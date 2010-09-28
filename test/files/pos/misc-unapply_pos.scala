object Test {
  val xs = List(1)
  val f: Int = {
    xs match {
      case List(x) => x
    }
  }
}

// the following comes from ticket #230
trait Foo {
  def name: String
  def unapply(x: String): Option[Unit] = {
    if (x == name) Some(()) else None
  }
}
object Bar extends Foo { def name = "bar" }
object Baz extends Foo { def name = "baz" }

object Test_ {
  def matcher(s: String) = s match {
    case Bar(x) => println("bar")
    case Baz(x) => println("baz")
//                          ^
// error: unreachable code
   }
 }
