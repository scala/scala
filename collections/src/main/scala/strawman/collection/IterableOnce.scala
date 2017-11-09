package strawman
package collection

import scala.{Any, Int, Unit, deprecated, `inline`, AnyVal, Boolean, Array, Option}
import scala.Predef.String
import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
  * A template trait for collections which can be traversed either once only
  * or one or more times.
  *
  * @define orderDependent
  *
  *    Note: might return different results for different runs, unless the underlying collection type is ordered.
  * @define orderDependentFold
  *
  *    Note: might return different results for different runs, unless the
  *    underlying collection type is ordered or the operator is associative
  *    and commutative.
  * @define mayNotTerminateInf
  *
  *    Note: may not terminate for infinite-sized collections.
  * @define willNotTerminateInf
  *
  *    Note: will not terminate for infinite-sized collections.
  *
  * @define coll collection
  */
trait IterableOnce[+A] extends Any {
  /** Iterator can be used only once */
  def iterator(): Iterator[A]

  /** @return The number of elements of this $coll if it can be computed in O(1) time, otherwise -1 */
  def knownSize: Int
}

final class IterableOnceExtensionMethods[A](private val it: IterableOnce[A]) extends AnyVal {
  @deprecated("Use .iterator().foreach(...) instead of .foreach(...) on IterableOnce", "2.13.0")
  @`inline` def foreach[U](f: A => U): Unit = it match {
    case it: Iterable[_] => it.asInstanceOf[Iterable[A]].foreach(f)
    case _ => it.iterator().foreach(f)
  }

  @deprecated("Use ArrayBuffer.from(it) instead of it.toBuffer", "2.13.0")
  def toBuffer[B >: A]: mutable.Buffer[B] = mutable.ArrayBuffer.from(it)

  @deprecated("Use ArrayBuffer.from(it).toArray", "2.13.0")
  def toArray[B >: A: ClassTag]: Array[B] = it match {
    case it: Iterable[B] => it.toArray[B]
    case _ => mutable.ArrayBuffer.from(it).toArray
  }

  @deprecated("Use List.from(it) instead of it.toList", "2.13.0")
  def toList[B >: A]: immutable.List[B] = immutable.List.from(it)

  @deprecated("Use .iterator().isEmpty instead of .isEmpty on IterableOnce", "2.13.0")
  def isEmpty: Boolean = it match {
    case it: Iterable[_] => it.isEmpty
    case _ => it.iterator().isEmpty
  }

  @deprecated("Use .iterator().mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString(start: String, sep: String, end: String): String = it match {
    case it: Iterable[_] => it.mkString(start, sep, end)
    case _ => it.iterator().mkString(start, sep, end)
  }

  @deprecated("Use .iterator().mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString(sep: String): String = it match {
    case it: Iterable[_] => it.mkString(sep)
    case _ => it.iterator().mkString(sep)
  }

  @deprecated("Use .iterator().mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString: String = it match {
    case it: Iterable[_] => it.mkString
    case _ => it.iterator().mkString
  }

  @deprecated("Use .iterator().find instead of .find on IterableOnce", "2.13.0")
  def find(p: A => Boolean): Option[A] = it.iterator().find(p)
}

object IterableOnce {
  @`inline` implicit def iterableOnceExtensionMethods[A](it: IterableOnce[A]): IterableOnceExtensionMethods[A] =
    new IterableOnceExtensionMethods[A](it)
}
