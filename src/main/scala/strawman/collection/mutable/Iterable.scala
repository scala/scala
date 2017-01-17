package strawman.collection.mutable
import strawman.collection

trait Iterable[A] extends collection.Iterable[A]
                     with collection.IterableLike[A, Iterable]
