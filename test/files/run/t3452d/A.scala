trait TraversableLike[A, Repr] {
  def tail: Repr = null.asInstanceOf[Repr]
}

abstract class AbstractTrav[A] extends TraversableLike[A, Iterable[A]]

class C[A] extends AbstractTrav[A]
