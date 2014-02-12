trait Cake extends Slice

// Minimization
trait Slice { self: Cake =>    // must have self type that extends `Slice`
  private[this] val bippy = () // must be private[this]
  locally(bippy)
  class C1 {
    locally(bippy)
    locally(self.bippy)
  }
}

// Originally reported bug:
trait Cake1 extends Slice1
trait Slice1 { self: Cake1 =>
  import java.lang.String // any import will do!
  val Tuple2(x, y) = ((1, 2))
}


// Nesting
trait Cake3 extends Outer.Slice3

// Minimization
object Outer {
  private[this] val bippy = ()
  trait Slice3 { self: Cake3 =>
    locally(bippy)
  }
}

object Test extends App {
  val s1 = new Cake1 {}
  assert((s1.x, s1.y) == (1, 2), (s1.x, s1.y))
}
