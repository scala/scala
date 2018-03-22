package scala.collection


package object mutable {
  @deprecated("Use WrappedArray instead of ArraySeq; it can represent both, boxed and unboxed arrays", "2.13.0")
  type ArraySeq[X] = WrappedArray[X]
  @deprecated("Use WrappedArray instead of ArraySeq; it can represent both, boxed and unboxed arrays", "2.13.0")
  val ArraySeq = WrappedArray.untagged
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[X] = Iterable[X]
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable
  @deprecated("Use Stack instead of ArrayStack; it now uses an array-based implementation", "2.13.0")
  type ArrayStack[X] = Stack[X]
  @deprecated("Use Stack instead of ArrayStack; it now uses an array-based implementation", "2.13.0")
  val ArrayStack = Stack

  @deprecated("mutable.LinearSeq has been removed; use LinearSeq with mutable.Seq instead", "2.13.0")
  type LinearSeq[X] = Seq[X] with scala.collection.LinearSeq[X]

  @deprecated("GrowingBuilder has been renamed to GrowableBuilder", "2.13.0")
  type GrowingBuilder[Elem, To <: Growable[Elem]] = GrowableBuilder[Elem, To]
}
