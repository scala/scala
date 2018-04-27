package scala.collection.mutable

import scala.collection.{IterableFactory, IterableOnce}
import scala.language.higherKinds

trait Iterable[A]
  extends collection.Iterable[A]
    with IterableOps[A, Iterable, Iterable[A]] {

  override def iterableFactory: IterableFactory[IterableCC] = Iterable
}

/**
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
trait IterableOps[A, +CC[X], +C]
  extends collection.IterableOps[A, CC, C] {

  /** Modifies this $coll by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       this $coll modified by replacing all elements with the
    *                result of applying the given function `f` to each element
    *                of this $coll.
    */
  def mapInPlace(f: A => A): this.type

  @deprecated("Use `mapInPlace` instead", "2.13.0")
  @`inline`final def transform(f: A => A): this.type = mapInPlace(f)
}

/**
  * $factoryInfo
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
object Iterable
  extends IterableFactory.Delegate[Iterable](ArrayBuffer)

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[A] extends scala.collection.AbstractIterable[A] with Iterable[A]
