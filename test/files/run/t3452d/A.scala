trait TraversableLike[A, Repr] {
  def tail: Repr = null.asInstanceOf[Repr]
}

abstract class AbstractTrav[A] extends TraversableLike[A, Traversable[A]]

class C[A] extends AbstractTrav[A]
