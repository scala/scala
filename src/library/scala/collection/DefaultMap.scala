/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._

/** <p>
 *    A default map which implements the <code>+</code> and <code>-</code>
 *    methods of maps.<br/>
 *    Instances that inherit from <code>DefaultMap[A, B]</code> still have to
 *    define:
 *  </p><pre>
 *    <b>def</b> get(key: A): Option[B]
 *    <b>def</b> iterator: Iterator[(A, B)]</pre>
 *  <p>
 *    It refers back to the original map.
 *  </p>
 *  <p>
 *    It might also be advisable to override <code>foreach</code> or
 *    <code>size</code> if efficient implementations can be found.
 *  </p>
 *
 *  @since 2.8
 */
trait DefaultMap[A, +B] extends Map[A, B] { self =>

  /** A default implementation which creates a new immutable map.
   */
  override def +[B1 >: B](kv: (A, B1)): Map[A, B1] = {
    val b = Map.newBuilder[A, B1]
    b ++= this
    b += ((kv._1, kv._2))
    b.result
  }

  /** A default implementation which creates a new immutable map.
   */
  override def - (key: A): Map[A, B] = {
    val b = newBuilder
    b ++= this filter (key !=)
    b.result
  }
}
