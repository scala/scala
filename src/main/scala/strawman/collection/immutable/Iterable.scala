package strawman.collection.immutable
import strawman.collection

trait Iterable[+A] extends collection.Iterable[A]
                      with collection.IterableLike[A, Iterable]
