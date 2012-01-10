/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package interfaces

trait TraversableOnceMethods[+A] {
  self: TraversableOnce[A] =>

  def foreach[U](f: A => U): Unit
  def size: Int
  protected[this] def reversed: TraversableOnce[A]

  // tests
  def hasDefiniteSize: Boolean
  def isEmpty: Boolean
  def isTraversableAgain: Boolean
  def nonEmpty: Boolean

  // applying a predicate
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B]
  def count(p: A => Boolean): Int
  def exists(p: A => Boolean): Boolean
  def find(p: A => Boolean): Option[A]
  def forall(p: A => Boolean): Boolean

  // folds
  def /:[B](z: B)(op: (B, A) => B): B
  def :\[B](z: B)(op: (A, B) => B): B
  def foldLeft[B](z: B)(op: (B, A) => B): B
  def foldRight[B](z: B)(op: (A, B) => B): B
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]
  def reduceLeft[B >: A](op: (B, A) => B): B
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]
  def reduceRight[B >: A](op: (A, B) => B): B

  // copies
  def copyToArray[B >: A](xs: Array[B]): Unit
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit
  def copyToBuffer[B >: A](dest: mutable.Buffer[B]): Unit

  // conversions
  def toArray[B >: A : ClassManifest]: Array[B]
  def toBuffer[B >: A]: mutable.Buffer[B]
  def toIndexedSeq: immutable.IndexedSeq[A]
  def toIterable: Iterable[A]
  def toIterator: Iterator[A]
  def toList: List[A]
  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U]
  def toSeq: Seq[A]
  def toSet[B >: A]: immutable.Set[B]
  def toStream: Stream[A]
  def toTraversable: Traversable[A]

  // type-constrained folds
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A
  def max[B >: A](implicit cmp: Ordering[B]): A
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A
  def min[B >: A](implicit cmp: Ordering[B]): A
  def product[B >: A](implicit num: Numeric[B]): B
  def sum[B >: A](implicit num: Numeric[B]): B

  // strings
  def mkString(start: String, sep: String, end: String): String
  def mkString(sep: String): String
  def mkString: String

  def addString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder
  def addString(buf: StringBuilder, sep: String): StringBuilder
  def addString(buf: StringBuilder): StringBuilder
}
