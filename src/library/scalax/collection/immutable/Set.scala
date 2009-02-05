/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Set.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.immutable

import collection.generic._

object Set extends generic.SetFactory[Set] {
  private val hashSeed = "Set".hashCode
  def empty[A]: Set[A] = null // !!!
}

trait Set[A] extends OrderedIterable[A]
                with collection.Set[A]
                with SetTemplate[Set, A] {

  /** Compares this set with another object and returns true, iff the
   *  other object is also a set which contains the same elements as
   *  this set.
   *
   *  @param that the other object
   *  @note not necessarily run-time type safe.
   *  @return     <code>true</code> iff this set and the other set
   *              contain the same elements.
   */
  override def equals(that: Any): Boolean = that match {
    case other: Set[_] =>
      this.size == other.size && subsetOf(other.asInstanceOf[Set[A]])
    case _ =>
      false
  }

  override def hashCode = (Set.hashSeed /: this)(_ * 41 + _.hashCode)

}
