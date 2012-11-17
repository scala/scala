trait Super[+A]
case class Concrete[Hidden, +A](havoc: Hidden => Hidden) extends Super[A]

object Test extends App {
  (Concrete((x: Int) => x): Super[Any]) match {
    case Concrete(f) => f("not what you'd expect")
  }
}