/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package interfaces

import mutable.Buffer

trait TraversableOnceMethods[+A] {
  self: TraversableOnce[A] =>

  def foreach[U](f: A => U): Unit
  protected[this] def reversed: TraversableOnce[A]

  // tests
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def hasDefiniteSize: Boolean
  def isTraversableAgain: Boolean

  // applying a predicate
  def forall(p: A => Boolean): Boolean
  def exists(p: A => Boolean): Boolean
  def find(p: A => Boolean): Option[A]
  def count(p: A => Boolean): Int

  // folds
  def /:[B](z: B)(op: (B, A) => B): B
  def :\[B](z: B)(op: (A, B) => B): B
  def foldLeft[B](z: B)(op: (B, A) => B): B
  def foldRight[B](z: B)(op: (A, B) => B): B
  def reduceLeft[B >: A](op: (B, A) => B): B
  def reduceRight[B >: A](op: (A, B) => B): B
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]

  // copies
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit
  def copyToArray[B >: A](xs: Array[B]): Unit

  // conversions
  def toArray[B >: A : ClassManifest]: Array[B]
  def toBuffer[B >: A]: mutable.Buffer[B]
  def toIndexedSeq[B >: A]: immutable.IndexedSeq[B]
  def toIterable: Iterable[A]
  def toIterator: Iterator[A]
  def toList: List[A]
  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U]
  def toSeq: Seq[A]
  def toSet[B >: A]: immutable.Set[B]
  def toStream: Stream[A]
  def toTraversable: Traversable[A]

  // type-constrained folds
  def sum[B >: A](implicit num: Numeric[B]): B
  def product[B >: A](implicit num: Numeric[B]): B
  def min[B >: A](implicit cmp: Ordering[B]): A
  def max[B >: A](implicit cmp: Ordering[B]): A

  // strings
  def mkString(start: String, sep: String, end: String): String
  def mkString(sep: String): String
  def mkString: String

  def addString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder
  def addString(buf: StringBuilder, sep: String): StringBuilder
  def addString(buf: StringBuilder): StringBuilder
}
