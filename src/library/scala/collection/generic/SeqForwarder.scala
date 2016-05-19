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
import scala.collection.immutable.Range

/** This class implements a forwarder for sequences. It forwards
 *  all calls to a different sequence object except for
 *
 *    - `toString`, `hashCode`, `equals`, `stringPrefix`
 *    - `newBuilder`, `view`, `toSeq`
 *    - all calls creating a new sequence of the same kind
 *
 *  The above methods are forwarded by subclass `SeqProxy`.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
@deprecated("forwarding is inherently unreliable since it is not automated and new methods can be forgotten", "2.11.0")
trait SeqForwarder[+A] extends Seq[A] with IterableForwarder[A] {

  protected override def underlying: Seq[A]

  override def length: Int = underlying.length
  override def apply(idx: Int): A = underlying.apply(idx)
  override def lengthCompare(len: Int): Int = underlying lengthCompare len
  override def isDefinedAt(x: Int): Boolean = underlying isDefinedAt x
  override def segmentLength(p: A => Boolean, from: Int): Int = underlying.segmentLength(p, from)
  override def prefixLength(p: A => Boolean) = underlying prefixLength p
  override def indexWhere(p: A => Boolean): Int = underlying indexWhere p
  override def indexWhere(p: A => Boolean, from: Int): Int = underlying.indexWhere(p, from)
  override def indexOf[B >: A](elem: B): Int = underlying indexOf elem
  override def indexOf[B >: A](elem: B, from: Int): Int = underlying.indexOf(elem, from)
  override def lastIndexOf[B >: A](elem: B): Int = underlying lastIndexOf elem
  override def lastIndexOf[B >: A](elem: B, end: Int): Int = underlying.lastIndexOf(elem, end)
  override def lastIndexWhere(p: A => Boolean): Int = underlying lastIndexWhere p
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = underlying.lastIndexWhere(p, end)
  override def reverseIterator: Iterator[A] = underlying.reverseIterator
  override def startsWith[B](that: GenSeq[B], offset: Int): Boolean = underlying.startsWith(that, offset)
  override def startsWith[B](that: GenSeq[B]): Boolean = underlying startsWith that
  override def endsWith[B](that: GenSeq[B]): Boolean = underlying endsWith that
  override def indexOfSlice[B >: A](that: GenSeq[B]): Int = underlying indexOfSlice that
  override def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int = underlying.indexOfSlice(that, from)
  override def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = underlying lastIndexOfSlice that
  override def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int = underlying.lastIndexOfSlice(that, end)
  override def containsSlice[B](that: GenSeq[B]): Boolean = underlying containsSlice that
  override def contains[A1 >: A](elem: A1): Boolean = underlying contains elem
  override def corresponds[B](that: GenSeq[B])(p: (A,B) => Boolean): Boolean = underlying.corresponds(that)(p)
  override def indices: Range = underlying.indices
}
