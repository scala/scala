package scala.collection.immutable

import scala.collection.IterableFactory

/** A trait for collections that are guaranteed immutable.
  *
  * @tparam A the element type of the collection
  *
  * @define usesMutableState
  *
  *   Note: Despite being an immutable collection, the implementation uses mutable state internally during
  *   construction. These state changes are invisible in single-threaded code but can lead to race conditions
  *   in some multi-threaded scenarios. The state of a new collection instance may not have been "published"
  *   (in the sense of the Java Memory Model specification), so that an unsynchronized non-volatile read from
  *   another thread may observe the object in an invalid state (see
  *   [[https://github.com/scala/bug/issues/7838 scala/bug#7838]] for details). Note that such a read is not
  *   guaranteed to ''ever'' see the written object at all, and should therefore not be used, regardless
  *   of this issue. The easiest workaround is to exchange values between threads through a volatile var.
  *
  * @define coll immutable collection
  * @define Coll `immutable.Iterable`
  */
trait Iterable[+A] extends collection.Iterable[A]
                      with collection.IterableOps[A, Iterable, Iterable[A]] {

  override def iterableFactory: IterableFactory[IterableCC] = Iterable
}

@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](List)
