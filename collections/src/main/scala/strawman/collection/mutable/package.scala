package strawman.collection

import scala.deprecated

package object mutable {
  @deprecated("Use WrappedArray instead of ArraySeq", "2.13.0")
  type ArraySeq[X] = WrappedArray[X]
  @deprecated("Use WrappedArray instead of ArraySeq", "2.13.0")
  val ArraySeq = WrappedArray
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[X] = Iterable[X]
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable

  @deprecated("mutable.LinearSeq has been removed; use LinearSeq with mutable.Seq instead", "2.13.0")
  type LinearSeq[X] = Seq[X] with strawman.collection.LinearSeq[X]

  @deprecated("GrowingBuilder has been renamed to GrowableBuilder", "2.13.0")
  type GrowingBuilder[Elem, To <: Growable[Elem]] = GrowableBuilder[Elem, To]
}
