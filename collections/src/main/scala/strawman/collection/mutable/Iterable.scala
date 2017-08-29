package strawman.collection.mutable

import strawman.collection
import strawman.collection.{IterableFactory, IterableOnce}

import scala.Boolean

trait Iterable[A]
  extends collection.Iterable[A]
    with IterableOps[A, Iterable, Iterable[A]]

/**
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
trait IterableOps[A, +CC[X], +C]
  extends collection.IterableOps[A, CC, C] {

  def mapInPlace(f: A => A): this.type
}

/**
  * $factoryInfo
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
object Iterable
  extends IterableFactory.Delegate[Iterable](ArrayBuffer)

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[A] extends strawman.collection.AbstractIterable[A] with Iterable[A]
