package strawman.collection.mutable

import strawman.collection
import strawman.collection.IterableFactory

trait Iterable[A] extends collection.Iterable[A]
                     with IterableOps[A, Iterable, Iterable[A]]

trait IterableOps[A, +CC[X], +C]
  extends collection.IterableOps[A, CC, C]

object Iterable
  extends IterableFactory.Delegate[Iterable](ArrayBuffer)
