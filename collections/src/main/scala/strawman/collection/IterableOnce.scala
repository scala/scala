package strawman
package collection

import scala.{Any, Int, Unit, deprecated, `inline`, AnyVal, Boolean, Array, Option}
import scala.Predef.String
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait IterableOnce[+A] extends Any {
  /** Iterator can be used only once */
  def iterator(): Iterator[A]

  def knownSize: Int
}

final class IterableOnceExtensionMethods[A](private val it: IterableOnce[A]) extends AnyVal {
  @deprecated("Use .iterator().foreach(...) instead of .foreach(...) on IterableOnce", "2.13.0")
  @`inline` def foreach[U](f: A => U): Unit = it match {
    case it: Iterable[_] => it.asInstanceOf[Iterable[A]].foreach(f)
    case _ => it.iterator().foreach(f)
  }

  @deprecated("Use ArrayBuffer.fromIterable(it) instead of it.toBuffer on Iterable (wrap Iterators with View.fromIterator first)", "2.13.0")
  def toBuffer[B >: A]: mutable.Buffer[B] = it match {
    case it: Iterable[_] => mutable.ArrayBuffer.from(it)
    case _ => mutable.ArrayBuffer.from(View.fromIteratorProvider(() => it.iterator()))
  }

  @deprecated("Use ArrayBuffer.fromIterable(it).toArray instead of it.toArray on Iterable (wrap Iterators with View.fromIterator first)", "2.13.0")
  def toArray[B >: A: ClassTag]: Array[B] = it match {
    case it: Iterable[_] => it.asInstanceOf[Iterable[B]].toArray[B]
    case _ => mutable.ArrayBuffer.from(View.fromIteratorProvider(() => it.iterator())).toArray
  }

  @deprecated("Use List.fromIterable(it) instead of it.toList on Iterable (wrap Iterators with View.fromIterator first)", "2.13.0")
  def toList[B >: A]: immutable.List[B] = it match {
    case it: Iterable[_] => immutable.List.from(it.asInstanceOf[Iterable[B]])
    case _ => immutable.List.from(View.fromIteratorProvider(() => it.iterator()))
  }

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
