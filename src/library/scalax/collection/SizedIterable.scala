/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Collection.scala 12340 2007-07-17 15:29:47Z mcdirmid $


package scalax.collection

/** Variant of <code>Iterable</code> which also demands
 *  implementation of a `size` method.
 *  Basically, this trait just adds size to Iterable,
 *  and provides an optimized implementation of toArray based on it.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait SizedIterable[+A] extends Iterable[A] {

  /** Returns the number of elements in this collection.
    *
    *  @return number of collection elements.
    */
  def size : Int

  /** Converts this iterable to a fresh Array with <code>size</code> elements.
    */
  override def toArray[B >: A]: Array[B] = {
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }
}

