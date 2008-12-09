package scalax.collection.generic.covariant

trait IterableFactory[CC[+A] <: Iterable[A]] extends generic.IterableFactory[CC] {

  /** The empty collection of type CC */
  val empty: CC[Nothing]

  override protected def newBuilder[A]: Builder[CC, A] =
    empty.newBuilder[A].asInstanceOf[Builder[CC, A]]
     // the cast here is unavoidable because CC is not constrained with covariant.IterableTemplate[CC, A]
     // It's can't be constrained because some suntype links between covariant and generic Templates
     // are missing. That's a consequence of our hacks to have both nonvariant and covariant templates.

  /** Create CC collection of specified elements */
  override def apply[A](args: A*): CC[A] =
    (empty ++ args.asInstanceOf[Iterable[A]]).asInstanceOf[CC[A]]
     // the cast here is unavoidable because CC is not constrained with covariant.IterableTemplate[CC, A]
     // It's can't be constrained because some suntype links between covariant and generic Templates
     // are missing. That's a consequence of our hacks to have both nonvariant and covariant templates.
}
