trait Component

class Owner {
  object Timestamp extends Component {
    def unapply(name: Timestamp.type): Boolean = true
  }
 
  class Inner {
    def foo(c: Component) = c match {
      case Timestamp() =>
    }
  }
}

object Test extends App {
  val outer = new Owner
  val inner = new outer.Inner
  inner.foo(outer.Timestamp)
}
