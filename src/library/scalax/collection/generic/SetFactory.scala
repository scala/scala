package scalax.collection.generic

trait SetFactory[CC[A] <: SetTemplate[CC, A] with Set[A]] extends IterableFactory[CC] {

  def empty[A]: CC[A]

  def apply[A](elems: A*): CC[A] = empty[A] ++ elems.asInstanceOf[Iterable[A]] // !@!

}
