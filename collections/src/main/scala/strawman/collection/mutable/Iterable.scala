package strawman.collection.mutable

import strawman.collection
import strawman.collection.{IterableFactory, IterableOnce}

import scala.Boolean

trait Iterable[A]
  extends collection.Iterable[A]
    with IterableOps[A, Iterable, Iterable[A]]

trait IterableOps[A, +CC[X], +C]
  extends collection.IterableOps[A, CC, C]
    with Growable[A] {

  def mapInPlace(f: A => A): this.type

  def flatMapInPlace(f: A => IterableOnce[A]): this.type

  def filterInPlace(p: A => Boolean): this.type

}

object Iterable
  extends IterableFactory.Delegate[Iterable](ArrayBuffer)
