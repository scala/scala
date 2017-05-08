package strawman.collection.mutable
import strawman.collection

trait Iterable[A] extends collection.Iterable[A]
                     with collection.IterableOps[A, Iterable, Iterable[A]]
