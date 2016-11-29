/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import scala.collection._
import mutable.{ Buffer, StringBuilder }
import immutable.{ List, Stream }
import scala.reflect.ClassTag

/** This trait implements a forwarder for traversable objects. It forwards
 *  all calls to a different traversable, except for:
 *
 *  - `toString`, `hashCode`, `equals`, `stringPrefix`
 *  - `newBuilder`, `view`
 *
 *  All calls creating a new traversable of the same kind.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("forwarding is inherently unreliable since it is not automated and new methods can be forgotten", "2.11.0")
trait TraversableForwarder[+A] extends Traversable[A] {
  /** The traversable object to which calls are forwarded. */
  protected def underlying: Traversable[A]

  override def foreach[U](f: A => U): Unit = underlying foreach f
  override def isEmpty: Boolean = underlying.isEmpty
  override def nonEmpty: Boolean = underlying.nonEmpty
  override def size: Int = underlying.size
  override def hasDefiniteSize = underlying.hasDefiniteSize
  override def forall(p: A => Boolean): Boolean = underlying forall p
  override def exists(p: A => Boolean): Boolean = underlying exists p
  override def count(p: A => Boolean): Int = underlying count p
  override def find(p: A => Boolean): Option[A] = underlying find p
  override def foldLeft[B](z: B)(op: (B, A) => B): B = underlying.foldLeft(z)(op)
  override def /: [B](z: B)(op: (B, A) => B): B = underlying./:(z)(op)
  override def foldRight[B](z: B)(op: (A, B) => B): B = underlying.foldRight(z)(op)
  override def :\ [B](z: B)(op: (A, B) => B): B = underlying.:\(z)(op)
  override def reduceLeft[B >: A](op: (B, A) => B): B = underlying.reduceLeft(op)
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = underlying.reduceLeftOption(op)
  override def reduceRight[B >: A](op: (A, B) => B): B = underlying.reduceRight(op)
  override def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = underlying.reduceRightOption(op)
  override def sum[B >: A](implicit num: Numeric[B]): B = underlying sum num
  override def product[B >: A](implicit num: Numeric[B]): B = underlying product num
  override def min[B >: A](implicit cmp: Ordering[B]): A = underlying min cmp
  override def max[B >: A](implicit cmp: Ordering[B]): A = underlying max cmp
  override def head: A = underlying.head
  override def headOption: Option[A] = underlying.headOption
  override def last: A = underlying.last
  override def lastOption: Option[A] = underlying.lastOption
  override def copyToBuffer[B >: A](dest: Buffer[B]) = underlying.copyToBuffer(dest)
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) = underlying.copyToArray(xs, start, len)
  override def copyToArray[B >: A](xs: Array[B], start: Int) = underlying.copyToArray(xs, start)
  override def copyToArray[B >: A](xs: Array[B]) = underlying.copyToArray(xs)
  override def toArray[B >: A: ClassTag]: Array[B] = underlying.toArray
  override def toList: List[A] = underlying.toList
  override def toIterable: Iterable[A] = underlying.toIterable
  override def toSeq: Seq[A] = underlying.toSeq
  override def toIndexedSeq = underlying.toIndexedSeq
  override def toBuffer[B >: A] = underlying.toBuffer
  override def toStream: Stream[A] = underlying.toStream
  override def toSet[B >: A]: immutable.Set[B] = underlying.toSet
  override def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = underlying.toMap(ev)
  override def mkString(start: String, sep: String, end: String): String = underlying.mkString(start, sep, end)
  override def mkString(sep: String): String = underlying.mkString(sep)
  override def mkString: String = underlying.mkString
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.addString(b, start, sep, end)
  override def addString(b: StringBuilder, sep: String): StringBuilder = underlying.addString(b, sep)
  override def addString(b: StringBuilder): StringBuilder = underlying.addString(b)
}
