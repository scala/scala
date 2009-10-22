/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.{Builder, Buffer, ArrayBuffer, ListBuffer}
import scala.util.control.Breaks

/** <p>
 *    A template trait for traversable collections.
 *  </p>
 *  <p>
 *    Collection classes mixing in this trait provide a method
 *    <code>foreach</code> which traverses all the
 *    elements contained in the collection, applying a given procedure to each.
 *    They also provide a method <code>newBuilder</code>
 *    which creates a builder for collections of the same kind.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait Traversable[+A] extends TraversableLike[A, Traversable[A]]
                         with GenericTraversableTemplate[A, Traversable] {
  def companion: GenericCompanion[Traversable] = Traversable

  /* The following methods are inherited from TraversableLike
   *
  override def isEmpty: Boolean
  override def size: Int
  override def hasDefiniteSize
  override def ++[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[Traversable[A], B, That]): That
  override def ++[B >: A, That](that: Iterator[B])(implicit bf: CanBuildFrom[Traversable[A], B, That]): That
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Traversable[A], B, That]): That
  override def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[Traversable[A], B, That]): That
  override def filter(p: A => Boolean): Traversable[A]
  override def remove(p: A => Boolean): Traversable[A]
  override def partition(p: A => Boolean): (Traversable[A], Traversable[A])
  override def groupBy[K](f: A => K): Map[K, Traversable[A]]
  override def foreach[U](f: A =>  U): Unit
  override def forall(p: A => Boolean): Boolean
  override def exists(p: A => Boolean): Boolean
  override def count(p: A => Boolean): Int
  override def find(p: A => Boolean): Option[A]
  override def foldLeft[B](z: B)(op: (B, A) => B): B
  override def /: [B](z: B)(op: (B, A) => B): B
  override def foldRight[B](z: B)(op: (A, B) => B): B
  override def :\ [B](z: B)(op: (A, B) => B): B
  override def reduceLeft[B >: A](op: (B, A) => B): B
  override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]
  override def reduceRight[B >: A](op: (A, B) => B): B
  override def reduceRightOption[B >: A](op: (A, B) => B): Option[B]
  override def head: A
  override def headOption: Option[A]
  override def tail: Traversable[A]
  override def last: A
  override def lastOption: Option[A]
  override def init: Traversable[A]
  override def take(n: Int): Traversable[A]
  override def drop(n: Int): Traversable[A]
  override def slice(from: Int, until: Int): Traversable[A]
  override def takeWhile(p: A => Boolean): Traversable[A]
  override def dropWhile(p: A => Boolean): Traversable[A]
  override def span(p: A => Boolean): (Traversable[A], Traversable[A])
  override def splitAt(n: Int): (Traversable[A], Traversable[A])
  override def copyToBuffer[B >: A](dest: Buffer[B])
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int)
  override def copyToArray[B >: A](xs: Array[B], start: Int)
  override def toArray[B >: A : ClassManifest]: Array[B]
  override def toList: List[A]
  override def toIterable: Iterable[A]
  override def toSeq: Seq[A]
  override def toStream: Stream[A]
//  override def sortWith(lt : (A,A) => Boolean): Traversable[A]
  override def mkString(start: String, sep: String, end: String): String
  override def mkString(sep: String): String
  override def mkString: String
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder
  override def addString(b: StringBuilder, sep: String): StringBuilder
  override def addString(b: StringBuilder): StringBuilder
  override def toString
  override def stringPrefix : String
  override def view
  override def view(from: Int, until: Int): TraversableView[A, Traversable[A]]
  */
}

/** Factory methods and utilities for instances of type <code>Traversable</code>.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
object Traversable extends TraversableFactory[Traversable] { self =>

  /** provide break functionality separate from client code */
  private[collection] val breaks: Breaks = new Breaks

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Traversable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Traversable[A]] = immutable.Traversable.newBuilder[A]
}

