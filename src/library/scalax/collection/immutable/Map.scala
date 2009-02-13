/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Set.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.immutable

import generic._

object Map extends MapFactory[Map] {
  private val hashSeed = "Map".hashCode
  def empty[A, B]: Map[A, B] = new EmptyMap[A, B]
}

trait Map[A, B] extends MapTemplate[A, B, Map] with collection.Map[A, B] { self =>

  def empty[B]: Map[A, B] = new EmptyMap[A, B]

  /** The same map with a given default function */
  def withDefault(d: A => B): Map[A, B] = new Map[A, B] {
    def size = self.size
    def get(key: A) = self.get(key)
    def elements = self.elements
    override def empty[C] = self.empty
    def update (key: A, value: B): Map[A, B] =
      self update (key, value) withDefault d
    def - (key: A): Map[A, B] =
      self - key withDefault d
    override def default(key: A): B = d(key)
  }

  /** The same map with a given default value */
  def withDefaultValue(d: B): Map[A, B] = withDefault(x => d)

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
    case other: Map[a, b] =>
      this.size == other.size && this.forall {
        case (key, value) => other.get(key.asInstanceOf[a]) match {
          case None => false
          case Some(otherval) => value == otherval
        }
      }
    case _ => false
  }

  /** A hash method compatible with <code>equals</code>
   */
  override def hashCode() =
    (Map.hashSeed /: this) (_ * 41 + _.hashCode)

}
