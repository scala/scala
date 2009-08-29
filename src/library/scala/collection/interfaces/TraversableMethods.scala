/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.interfaces

import scala.collection._
import generic._
import mutable.Buffer
import scala.reflect.ClassManifest

trait TraversableMethods[+A, +This <: TraversableTemplate[A, This] with Traversable[A]]
{
  // maps/iteration
  def flatMap[B, That](f: A => Traversable[B])(implicit bf: BuilderFactory[B, That, This]): That
  def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, This]): That
  def filterMap[B, That](pf: PartialFunction[Any, B])(implicit bf: BuilderFactory[B, That, This]): That
  def foreach[U](f: A => U): Unit

  // new collections
  def ++[B >: A, That](that: Iterator[B])(implicit bf: BuilderFactory[B, That, This]): That
  def ++[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, This]): That
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit

  // conversions
  def toArray[B >: A : ClassManifest]: Array[B]
  def toIterable: Iterable[A]
  def toList: List[A]
  def toSequence: Sequence[A]
  def toStream: Stream[A]

  // strings
  def addString(b: StringBuilder): StringBuilder
  def addString(b: StringBuilder, sep: String): StringBuilder
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder
  def mkString(sep: String): String
  def mkString(start: String, sep: String, end: String): String
  def mkString: String

  // folds
  def /: [B](z: B)(op: (B, A) => B): B
  def :\ [B](z: B)(op: (A, B) => B): B
  def foldLeft[B](z: B)(op: (B, A) => B): B
  def foldRight[B](z: B)(op: (A, B) => B): B
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]
  def reduceLeft[B >: A](op: (B, A) => B): B
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]
  def reduceRight[B >: A](op: (A, B) => B): B

  // conditions
  def exists(p: A => Boolean): Boolean
  def forall(p: A => Boolean): Boolean
  def hasDefiniteSize: Boolean
  def isEmpty: Boolean
  def nonEmpty: Boolean

  // element retrieval
  def find(p: A => Boolean): Option[A]
  def head: A
  def headOption: Option[A]
  def last: A
  def lastOption: Option[A]

  // subcollections
  def drop(n: Int): Traversable[A]
  def dropWhile(p: A => Boolean): Traversable[A]
  def filter(p: A => Boolean): Traversable[A]
  def filterNot(p: A => Boolean): Traversable[A]
  def init: Traversable[A]
  def slice(from: Int, until: Int): Traversable[A]
  def tail: Traversable[A]
  def take(n: Int): Traversable[A]
  def takeWhile(p: A => Boolean): Traversable[A]

  // subdivisions
  def groupBy[K](f: A => K): Map[K, Traversable[A]]
  def partition(p: A => Boolean): (Traversable[A], Traversable[A])
  def span(p: A => Boolean): (Traversable[A], Traversable[A])
  def splitAt(n: Int): (Traversable[A], Traversable[A])

  // info
  def count(p: A => Boolean): Int
  def size: Int
  def stringPrefix: String

  // views
  def view: TraversableView[A, This]
  def view(from: Int, until: Int): TraversableView[A, This]

  // def sum[B >: A](implicit num: Numeric[B]): B
  // def product[B >: A](implicit num: Numeric[B]): B
  // def min[B >: A](implicit cmp: Ordering[B]): A
  // def max[B >: A](implicit cmp: Ordering[B]): A
}
