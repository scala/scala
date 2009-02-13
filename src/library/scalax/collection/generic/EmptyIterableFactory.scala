package scalax.collection.generic

import annotation.unchecked.uncheckedVariance

trait EmptyIterableFactory[CC[+A] <: Iterable[A] with IterableTemplate[CC, A @uncheckedVariance]] extends IterableFactory[CC] {

  /** The empty collection of type CC */
  val empty: CC[Nothing]

  /** An override of newBuilder, to work off the empty object */
  override protected def newBuilder[A]: Builder[CC, A] =
    empty.newBuilder[A]

  /** Create CC collection of specified elements */
  override def apply[A](args: A*): CC[A] =
    empty ++ args.asInstanceOf[Iterable[A]]
}
