package scalax.collection.generic.covariant

trait SequenceFactory[CC[+A] <: Sequence[A]] extends IterableFactory[CC] with generic.SequenceFactory[CC]
