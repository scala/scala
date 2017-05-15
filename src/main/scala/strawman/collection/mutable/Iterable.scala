package strawman.collection.mutable

import strawman.collection
import strawman.collection.{IterableFactory, IterableOnce}

import scala.Boolean

trait Iterable[A]
  extends collection.Iterable[A]
    with IterableOps[A, Iterable, Iterable[A]] {

    def mapInPlace(f: A => A): this.type

}

trait IterableOps[A, +CC[X], +C]
  extends collection.IterableOps[A, CC, C]

object Iterable
  extends IterableFactory.Delegate[Iterable](ArrayBuffer)

trait GrowableIterable[A]
  extends Iterable[A]
    with Growable[A] {

  def flatMapInPlace(f: A => IterableOnce[A]): this.type

  def filterInPlace(p: A => Boolean): this.type

}
