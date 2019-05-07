trait TraversableLike[A, Repr] {
  def tail: Repr = null.asInstanceOf[Repr]
}

abstract class AbstractTrav[A] extends TraversableLike[A, AbstractTrav[A]]

object O extends AbstractTrav[String]

class C[A] extends AbstractTrav[A]
