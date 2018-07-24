package scala.collection.mutable

import scala.collection.IterableFactory

trait Iterable[A]
  extends collection.Iterable[A]
    with collection.IterableOps[A, Iterable, Iterable[A]] {

  override def iterableFactory: IterableFactory[IterableCC] = Iterable
}

/**
  * $factoryInfo
  * @define coll mutable collection
  * @define Coll `mutable.Iterable`
  */
@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](ArrayBuffer)

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractIterable[A] extends scala.collection.AbstractIterable[A] with Iterable[A]
