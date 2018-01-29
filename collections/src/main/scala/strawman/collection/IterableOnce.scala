package strawman
package collection

import scala.{Any, Int, Unit, deprecated, `inline`, AnyVal, Boolean, Array, Option}
import scala.Predef.{ String, <:< }
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
    case it: Iterable[A] => it.foreach(f)
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
  def toList: immutable.List[A] = immutable.List.from(it)

  @deprecated("Use Set.from(it) instead of it.toSet", "2.13.0")
  @`inline` def toSet[B >: A]: immutable.Set[B] = immutable.Set.from(it)

  @deprecated("Use Seq.from(it) instead of it.toSeq", "2.13.0")
  @`inline` def toSeq: immutable.Seq[A] = immutable.Seq.from(it)

  @deprecated("Use LazyList.from(it) instead of it.toStream", "2.13.0")
  @`inline` def toStream: immutable.LazyList[A] = immutable.LazyList.from(it)

  @deprecated("Use Vector.from(it) instead of it.toVector on IterableOnce", "2.13.0")
  @`inline` def toVector: immutable.Vector[A] = immutable.Vector.from(it)

  @deprecated("Use Map.from(it) instead of it.toVector on IterableOnce", "2.13.0")
  def toMap[K, V](implicit ev: A <:< (K, V)): immutable.Map[K, V] =
    immutable.Map.from(it.asInstanceOf[IterableOnce[(K, V)]])

  @deprecated("toIterator has been renamed to iterator()", "2.13.0")
  @`inline` def toIterator: Iterator[A] = it.iterator()

  @deprecated("Use .iterator().isEmpty instead of .isEmpty on IterableOnce", "2.13.0")
  def isEmpty: Boolean = it match {
    case it: Iterable[A] => it.isEmpty
    case _ => it.iterator().isEmpty
  }

  @deprecated("Use .iterator().mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString(start: String, sep: String, end: String): String = it match {
    case it: Iterable[A] => it.mkString(start, sep, end)
    case _ => it.iterator().mkString(start, sep, end)
  }

  @deprecated("Use .iterator().mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString(sep: String): String = it match {
    case it: Iterable[A] => it.mkString(sep)
    case _ => it.iterator().mkString(sep)
  }

  @deprecated("Use .iterator().mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString: String = it match {
    case it: Iterable[A] => it.mkString
    case _ => it.iterator().mkString
  }

  @deprecated("Use .iterator().find instead of .find on IterableOnce", "2.13.0")
  def find(p: A => Boolean): Option[A] = it.iterator().find(p)

  @deprecated("Use .iterator().foldLeft instead of .foldLeft on IterableOnce", "2.13.0")
  @`inline` def foldLeft[B](z: B)(op: (B, A) => B): B = it.iterator().foldLeft(z)(op)

  @deprecated("Use .iterator().foldRight instead of .foldLeft on IterableOnce", "2.13.0")
  @`inline` def foldRight[B](z: B)(op: (A, B) => B): B = it.iterator().foldRight(z)(op)

  @deprecated("Use .iterator().foldLeft instead of /: on IterableOnce", "2.13.0")
  @`inline` def /: [B](z: B)(op: (B, A) => B): B = foldLeft[B](z)(op)

  @deprecated("Use .iterator().foldRight instead of :\\ on IterableOnce", "2.13.0")
  @`inline` def :\ [B](z: B)(op: (A, B) => B): B = foldRight[B](z)(op)

  @deprecated("Use .iterator().map instead of .map on IterableOnce or consider requiring an Iterable", "2.13.0")
  def map[B](f: A => B): IterableOnce[B] = it match {
    case it: Iterable[A] => it.asInstanceOf[Iterable[A]].map(f)
    case _ => it.iterator().map(f)
  }

  @deprecated("Use .iterator().flatMap instead of .flatMap on IterableOnce or consider requiring an Iterable", "2.13.0")
  def flatMap[B](f: A => IterableOnce[B]): IterableOnce[B] = it match {
    case it: Iterable[A] => it.asInstanceOf[Iterable[A]].flatMap(f)
    case _ => it.iterator().flatMap(f)
  }
}

object IterableOnce {
  @`inline` implicit def iterableOnceExtensionMethods[A](it: IterableOnce[A]): IterableOnceExtensionMethods[A] =
    new IterableOnceExtensionMethods[A](it)
}
