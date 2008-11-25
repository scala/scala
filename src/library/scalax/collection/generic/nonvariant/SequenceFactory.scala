package scalax.collection.generic.nonvariant

trait SequenceFactory[CC[A] <: Sequence[A]] extends IterableFactory[CC] with generic.SequenceFactory[CC]
