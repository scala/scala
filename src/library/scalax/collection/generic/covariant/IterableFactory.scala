package scalax.collection.generic.covariant

trait IterableFactory[CC[+A] <: Iterable[A]] extends generic.IterableFactory[CC] {

  /** The empty collection of type CC */
  val empty: CC[Nothing]

  override protected def newBuilder[A]: Builder[CC, A] =
    empty.newBuilder[A].asInstanceOf[Builder[CC, A]]

  /** Create CC collection of specified elements */
  override def apply[A](args: A*): CC[A] =
    (empty ++ args.asInstanceOf[Iterable[A]]).asInstanceOf[CC[A]] // !!!
}
