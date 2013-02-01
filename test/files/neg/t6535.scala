object As {
  import Bs.B._

  object A
  extends scala.AnyRef // needed for the cycle;
                       // replacing with a locally defined closs doesn't
                       // hit the locked import and hence doesn't cycle.
}

object Bs {
  import As.A._

  object B
  extends scala.AnyRef // scala.Immutable, ...
}
