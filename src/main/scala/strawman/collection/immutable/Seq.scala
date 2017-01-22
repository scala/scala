package strawman.collection.immutable

import strawman.collection

trait Seq[+A] extends collection.Seq[A]
                 with collection.SeqLike[A, Seq]
                 with Iterable[A]


