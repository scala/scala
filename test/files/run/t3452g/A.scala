trait TraversableLike[A, Repr] {
  def tail: Repr = null.asInstanceOf[Repr]
}

abstract class AbstractTrav[A] extends TraversableLike[A, AbstractTrav[A]]

class C1 extends AbstractTrav[String]

object O extends AbstractTrav[String]

class C2[A] extends AbstractTrav[A]
