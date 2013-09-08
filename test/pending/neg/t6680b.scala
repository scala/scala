trait Super[+A]
// `Hidden` must occur in both variance positions (covariant/contravariant) for the sneakiness to work
// this way type inference will infer Any for `Hidden` and `A` in the pattern below
case class Concrete[Hidden, +A](havoc: Hidden => Hidden) extends Super[A]

object Test extends App {
  (Concrete((x: Int) => x): Super[Any]) match {
    case Concrete(f) => f("not what you'd expect")
  }
}