/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.generic

/** A non-strict view of a map.
 * @author Martin Odersky
 * @version 2.8
 */
trait MapView[+UC[A1, B1] <: MapTemplate[A1, B1, UC] with Map[A1, B1], A, B] extends Map[A, B] { self =>

  val origin: Map[A, _]

  def empty[C] = origin.empty[C]

  def elements: Iterator[(A, B)]

  val underlying: Map[A, _] = origin match {
    case v: MapView[t, _, _] => v.underlying
    case _ => origin
  }
  private[this] var forced: UC[A, B] = _
  private[this] var wasForced = false

  def force: UC[A, B] = {
    if (!wasForced) {
      forced = underlying.empty[B].asInstanceOf[UC[A, B]] ++ elements
      wasForced = true
    }
    forced
  }

  def update (key: A, value: B): UC[A, B] = force update (key, value)
  def - (key: A): UC[A, B] = force - key
}


