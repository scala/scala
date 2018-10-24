/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

/** A default map which implements the `+` and `-`
 *  methods of maps. It does so using the default builder for
 *  maps defined in the `Map` object.
 *  Instances that inherit from `DefaultMap[A, B]` still have to
 *  define:
 *
 *  {{{
 *    def get(key: A): Option[B]
 *    def iterator: Iterator[(A, B)]
 *  }}}
 *
 *  It refers back to the original map.
 *
 *  It might also be advisable to override `foreach` or
 *  `size` if efficient implementations can be found.
 *
 *  @tparam A    the type of the keys contained in this map.
 *  @tparam B    the type of the values associated with the keys.
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
    b.result()
  }

  /** A default implementation which creates a new immutable map.
   */
  override def - (key: A): Map[A, B] = {
    val b = newBuilder
    for (kv <- this ; if kv._1 != key) b += kv
    b.result()
  }
}
