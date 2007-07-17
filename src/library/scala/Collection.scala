/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import Predef._

/** Variant of <code>Iterable</code> used to describe
 *  collections with a finite number of elements.
 *  Basically, this trait just adds size and toString to Iterable,
 *  as most of the methods in Iterable already assume finite-ness.
 *
 *  @author Sean McDirmid
 */
trait Collection[+A] extends Iterable[A] {
  /** Returns the number of elements in this collection.
    *
    *  @return number of collection elements.
    */
  def size : Int
  /** Converts this iterable to a fresh Array with elements.
    */
  def toArray[B >: A]: Array[B] = toList.toArray

  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  protected def stringPrefix : String = {
    var string = this.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }
}

